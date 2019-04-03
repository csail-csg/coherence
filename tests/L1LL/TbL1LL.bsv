
// Copyright (c) 2017 Massachusetts Institute of Technology
// 
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

import Types::*;
import MemoryTypes::*;
import Amo::*;
import CacheUtils::*;

import CCTypes::*;
import CCSizes::*;
import FShow::*;
import Randomizable::*;
import Vector::*;
import FIFO::*;
import RegFile::*;
import Connectable::*;
import GetPut::*;
import ClientServer::*;
import AtomicMem::*;
import DelayMemTypes::*;
import IdealDelayMem::*;
import Printf::*;
import ConfigReg::*;

import L1LL::*;

// FIXME assume no banking

// number of reqs (tests) per core
typedef 10000 TestNum;
typedef Bit#(TLog#(TestNum)) TestId;
typedef Bit#(TLog#(TAdd#(TestNum, 1))) TestCnt;

typedef TDiv#(TestNum, 10) TestPrintNum;

// number of DMA reqs
typedef 100 DmaTestNum; //TMul#(TestNum, L1Num) DmaTestNum; // To reduce sc failure
typedef Bit#(TLog#(DmaTestNum)) DmaTestId;
typedef Bit#(TLog#(TAdd#(DmaTestNum, 1))) DmaTestCnt;
typedef TDiv#(DmaTestNum, 10) DmaTestPrintNum;

// memory delay
typedef 30 MemDelay;

// time out
typedef 10000 MaxTimeOut;
typedef Bit#(TLog#(MaxTimeOut)) TimeOutCnt;

// test index, tag, data offset choices
typedef TMul#(4, LLWayNum) TagNum;
typedef 2 IndexNum;

typedef LLTag LLCTag;
typedef LLIndex LLCIndex;

function Addr getAddr(LLCTag tag, LLCIndex index, LineDataOffset sel);
    DataBytesOffset off = 0;
    return {tag, index, sel, off};
endfunction

function Addr getInstAddr(LLCTag tag, LLCIndex index, LineInstOffset sel);
    Bit#(TLog#(TDiv#(InstSz, 8))) off = 0;
    return {tag, index, sel, off};
endfunction

// memory size for testing
typedef TAdd#(TSub#(AddrSz, SizeOf#(LLCTag)), TLog#(TagNum)) LgTestMemSzBytes;

// req/resp to/from memory system
typedef enum {
    Ld, St, Lr, Sc, Amo
`ifdef STORE_PREFETCH
    , StPrefetch
`endif
} MemTestOp deriving(Bits, Eq, FShow, Bounded);

function MemOp getMemOp(MemTestOp op);
    case(op)
        Ld: return Ld;
        St: return St;
        Lr: return Lr;
        Sc: return Sc;
        Amo: return Amo;
`ifdef STORE_PREFETCH
        StPrefetch: return StPrefetch;
`endif
        default: return ?;
    endcase
endfunction

function Msi getToState(MemTestOp op);
    case(op)
        Ld: return S;
        Lr: return E;
        St, Sc, Amo: return M;
`ifdef STORE_PREFETCH
        StPrefetch: return E;
`endif
        default: return ?;
    endcase
endfunction

typedef struct {
    TestId id;
    MemTestOp op;
    Addr addr;
    ByteEn byteEn; // for Sc
    Data data; // for Sc/Amo
    LineByteEn lineBE; // for St
    Line line; // for St
    AmoInst amoInst; // for Amo
} MemTestReq deriving(Bits, Eq, FShow);

typedef enum {
    Ld, St, LrScAmo
`ifdef STORE_PREFETCH
    , StPrefetch
`endif
} MemRespType deriving(Bits, Eq, FShow);
typedef struct {
    MemRespType t;
    TestId id;
    Data data;
} MemTestResp deriving(Bits, Eq, FShow);

function MemRespType getMemRespType(MemTestOp op);
    case(op)
        Ld: return Ld;
        St: return St;
        Lr, Sc, Amo: return LrScAmo;
`ifdef STORE_PREFETCH
        StPrefetch: return StPrefetch;
`endif
        default: return ?;
    endcase
endfunction

// random req stall
typedef Bit#(2) ReqStall;

function Bool getReqStall(ReqStall x);
`ifdef NO_REQ_STALL
    return False;
`else
    return x == 0;
`endif
endfunction

function Bool getDmaReqStall(ReqStall x);
`ifdef NO_DMA_REQ_STALL
    return False;
`else
    return x == 0;
`endif
endfunction

// update cache line
// test FSM
typedef enum {InitTable, InitAddr, Idle, Process, Done} TestFSM deriving(Bits, Eq, FShow);

(* synthesize *)
module mkTbL1LL(Empty);
    // Reference
    AtomicMem#(LgTestMemSzBytes) refMem <- mkAtomicMem;
    Reg#(Vector#(L1DNum, Vector#(L1BankNum, Maybe#(LineAddr)))) refLink <- mkReg(replicate(replicate(Invalid)));

    // randomize req
    // D$
    Vector#(L1DNum, Randomize#(ReqStall)) randDCReqStall <- replicateM(mkGenericRandomizer);
    Vector#(L1DNum, Randomize#(MemTestOp)) randDCOp <- replicateM(mkConstrainedRandomizer(minBound, maxBound));
    Vector#(L1DNum, Randomize#(LLCTag)) randDCTag <- replicateM(mkConstrainedRandomizer(0, fromInteger(valueOf(TagNum) - 1)));
    Vector#(L1DNum, Randomize#(LLCIndex)) randDCIndex <- replicateM(mkConstrainedRandomizer(0, fromInteger(valueOf(IndexNum) - 1)));
    Vector#(L1DNum, Randomize#(LineDataOffset)) randDCDataSel <- replicateM(mkGenericRandomizer);
    Vector#(L1DNum, Randomize#(Data)) randDCData <- replicateM(mkGenericRandomizer);
    Vector#(L1DNum, Randomize#(Bit#(DataSzBytes))) randDCDataBE <- replicateM(mkConstrainedRandomizer(1, maxBound)); // it could be all 0 though..
    Vector#(L1DNum, Randomize#(Line)) randDCLine <- replicateM(mkGenericRandomizer);
    Vector#(L1DNum, Randomize#(Bit#(LineSzBytes))) randDCLineBE <- replicateM(mkConstrainedRandomizer(1, maxBound)); // it could be all 0 though..
    Vector#(L1DNum, Randomize#(AmoFunc)) randDCAmoFunc <- replicateM(mkConstrainedRandomizer(Swap, Maxu));
    Vector#(L1DNum, Randomize#(Bool)) randDCDoubleWord <- replicateM(mkGenericRandomizer);
    // I$
    Vector#(L1INum, Randomize#(ReqStall)) randICReqStall <- replicateM(mkGenericRandomizer);
    Vector#(L1INum, Randomize#(LLCTag)) randICTag <- replicateM(mkConstrainedRandomizer(0, fromInteger(valueOf(TagNum) - 1)));
    Vector#(L1INum, Randomize#(LLCIndex)) randICIndex <- replicateM(mkConstrainedRandomizer(0, fromInteger(valueOf(IndexNum) - 1)));
    Vector#(L1INum, Randomize#(LineInstOffset)) randICInstSel <- replicateM(mkGenericRandomizer);
    // DMA
    Randomize#(ReqStall) randDmaReqStall <- mkGenericRandomizer;
    Randomize#(Bool) randDmaWrite <- mkGenericRandomizer;
    Randomize#(Bit#(LineSzBytes)) randDmaBE <- mkConstrainedRandomizer(1, maxBound); // this cannot be all 0
    Randomize#(LLCTag) randDmaTag <- mkConstrainedRandomizer(0, fromInteger(valueOf(TagNum) - 1));
    Randomize#(LLCIndex) randDmaIndex <- mkConstrainedRandomizer(0, fromInteger(valueOf(IndexNum) - 1));
    Randomize#(Bit#(SizeOf#(Line))) randDmaData <- mkGenericRandomizer;

    // record req
    // D$
    Vector#(L1DNum, RegFile#(TestId, Maybe#(MemTestReq))) dcReqTable <- replicateM(mkRegFileFull);
    Vector#(L1DNum, Reg#(TestCnt)) sendDCCnt <- replicateM(mkReg(0));
    Vector#(L1DNum, RWire#(MemTestReq)) sendDCReq <- replicateM(mkRWire);
    // I$
    Vector#(L1INum, RegFile#(TestId, Maybe#(Addr))) icReqTable <- replicateM(mkRegFileFull);
    Vector#(L1INum, Reg#(TestCnt)) sendICCnt <- replicateM(mkReg(0));
    Vector#(L1INum, RWire#(Addr)) sendICReq <- replicateM(mkRWire);
    // DMA
    RegFile#(DmaTestId, Maybe#(DmaRq#(DmaTestId))) dmaReqTable <- mkRegFileFull;
    Reg#(DmaTestCnt) sendDmaCnt <- mkReg(0);
    RWire#(DmaRq#(DmaTestId)) sendDmaReq <- mkRWire;

    // print helper for send: print when read this number
    Vector#(L1DNum, Reg#(TestCnt)) sendPrintDCCnt <- replicateM(mkReg(fromInteger(valueOf(TestPrintNum))));
    Vector#(L1INum, Reg#(TestCnt)) sendPrintICCnt <- replicateM(mkReg(fromInteger(valueOf(TestPrintNum))));
    Reg#(DmaTestCnt) sendPrintDmaCnt <- mkReg(fromInteger(valueof(DmaTestPrintNum)));
    
    // record resp
    // D$
    Vector#(L1DNum, RegFile#(TestId, Bool)) dcRespDoneTable <- replicateM(mkRegFileFull);
    Vector#(L1DNum, Reg#(TestCnt)) recvDCCnt <- replicateM(mkReg(0));
    Vector#(L1DNum, RWire#(MemTestResp)) recvDCResp <- replicateM(mkRWire);
    Vector#(L1DNum, Reg#(TimeOutCnt)) dcTimeOut <- replicateM(mkReg(0));
    // I$ (in order resp)
    Vector#(L1INum, Reg#(TestCnt)) recvICCnt <- replicateM(mkReg(0));
    Vector#(L1INum, RWire#(L1InstResult)) recvICResp <- replicateM(mkRWire);
    Vector#(L1INum, Reg#(TimeOutCnt)) icTimeOut <- replicateM(mkReg(0));
    // DMA
    RegFile#(DmaTestId, Bool) dmaRespDoneTable <- mkRegFileFull;
    Reg#(DmaTestCnt) recvDmaCnt <- mkReg(0);
    RWire#(DmaRs#(DmaTestId)) recvDmaResp <- mkRWire;
    Reg#(TimeOutCnt) dmaTimeOut <- mkReg(0);

    // resp for I$ req really taking effects
    Vector#(L1INum, RegFile#(TestId, Maybe#(L1InstResult))) icRefTable <- replicateM(mkRegFileFull);
    Vector#(L1INum, RWire#(TestId)) recvICDone <- replicateM(mkRWire);

    // resp for DMA req really taking effects
    RegFile#(DmaTestId, Bool) dmaRefWrMissTable <- mkRegFileFull;
    RegFile#(DmaTestId, Bool) dmaRefWrHitTable <- mkRegFileFull;
    RegFile#(DmaTestId, Maybe#(Line)) dmaRefRdMissTable <- mkRegFileFull;
    RegFile#(DmaTestId, Maybe#(Line)) dmaRefRdHitTable <- mkRegFileFull;
    RWire#(DmaTestId) recvDmaWrMiss <- mkRWire;
    RWire#(DmaTestId) recvDmaWrHit <- mkRWire;
    RWire#(DmaTestId) recvDmaRdMiss <- mkRWire;
    RWire#(DmaTestId) recvDmaRdHit <- mkRWire;

    // check cononicalize rule fires
    PulseWire processFired <- mkPulseWire;

    // log files
    Vector#(L1DNum, Reg#(File)) dcReqLog <- replicateM(mkReg(InvalidFile));
    Vector#(L1DNum, Reg#(File)) dcRespLog <- replicateM(mkReg(InvalidFile));

    Vector#(L1INum, Reg#(File)) icReqLog <- replicateM(mkReg(InvalidFile));
    Vector#(L1INum, Reg#(File)) icRespLog <- replicateM(mkReg(InvalidFile));

    Reg#(File) dmaReqLog <- mkReg(InvalidFile);
    Reg#(File) dmaRespLog <- mkReg(InvalidFile);

    // init
    Reg#(TestFSM) testFSM <- mkConfigReg(InitTable);
    Reg#(Bool) coreTableInitDone <- mkReg(False);
    Reg#(Bool) dmaTableInitDone <- mkReg(False);
    Reg#(TestId) iterId <- mkReg(0);
    Reg#(DmaTestId) iterDmaId <- mkReg(0);
    Reg#(LLCTag) iterTag <- mkReg(0);
    Reg#(LLCIndex) iterIndex <- mkReg(0);

    // stats
    Vector#(L1DNum, Vector#(L1BankNum, Reg#(Data))) ldCnt <- replicateM(replicateM(mkReg(0)));
    Vector#(L1DNum, Vector#(L1BankNum, Reg#(Data))) stCnt <- replicateM(replicateM(mkReg(0)));
    Vector#(L1DNum, Vector#(L1BankNum, Reg#(Data))) amoCnt <- replicateM(replicateM(mkReg(0)));
    Vector#(L1DNum, Vector#(L1BankNum, Reg#(Data))) lrCnt <- replicateM(replicateM(mkReg(0)));
    Vector#(L1DNum, Vector#(L1BankNum, Reg#(Data))) scCnt <- replicateM(replicateM(mkReg(0)));
    Vector#(L1DNum, Vector#(L1BankNum, Reg#(Data))) scSuccCnt <- replicateM(replicateM(mkReg(0)));
    Vector#(L1DNum, Vector#(L1BankNum, Reg#(Data))) scFailCnt <- replicateM(replicateM(mkReg(0)));
`ifdef STORE_PREFETCH
    Vector#(L1DNum, Vector#(L1BankNum, Reg#(Data))) stPrefetchCnt <- replicateM(replicateM(mkReg(0)));
`endif

    // DUT
    function L1ProcResp#(ProcRqId) getL1ProcResp(Integer i);
        return (interface L1ProcResp;
            method Action respLd(ProcRqId id, Data d);
                recvDCResp[i].wset(MemTestResp {t: Ld, id: truncate(id), data: d});
            endmethod
            method Action respLrScAmo(ProcRqId id, Data d);
                recvDCResp[i].wset(MemTestResp {t: LrScAmo, id: truncate(id), data: d});
            endmethod
            method ActionValue#(Tuple2#(LineByteEn, Line)) respSt(ProcRqId id);
                recvDCResp[i].wset(MemTestResp {t: St, id: truncate(id), data: ?});
                let req = validValue(dcReqTable[i].sub(truncate(id)));
                return tuple2(req.lineBE, req.line);
            endmethod
`ifdef DEBUG_STORE_PREFETCH
            method Action respStPrefetch(ProcRqId id);
                recvDCResp[i].wset(MemTestResp {t: StPrefetch, id: truncate(id), data: ?});
            endmethod
`endif
            method Action evict(LineAddr a);
                noAction;
            endmethod
        endinterface);
    endfunction
    let memSys <- mkL1LL(map(getL1ProcResp, genVector));
    IdealDelayMem#(MemDelay, LgTestMemSzBytes, LdMemRqId#(LLCRqMshrIdx), void) delayMem <- mkIdealDelayMem;
    mkConnection(memSys.to_mem, delayMem.to_proc);

    // XXX add 1 cycle delay to I$ resp, so done comes at least 1 cycle before resp
    Vector#(L1INum, FIFO#(L1InstResult)) icDelayQ <- replicateM(mkFIFO);
    for(Integer i = 0; i < valueof(L1INum); i = i+1) begin
        mkConnection(toPut(icDelayQ[i]), memSys.inst[i].resp);
    end

    // get all ifc used in the reset of the testbench
    Vector#(L1DNum, L1ProcReq#(ProcRqId)) ifcDC = memSys.dReq;
    Vector#(L1INum, InstServer#(L1ISupSz)) ifcIC = ?;
    for(Integer i = 0; i < valueof(L1INum); i = i+1) begin
        ifcIC[i] = (interface InstServer;
            interface req = memSys.inst[i].req;
            interface resp = toGet(icDelayQ[i]); // use delayed resp
            interface done = memSys.inst[i].done;
        endinterface);
    end
    DmaServer#(DmaRqId) ifcDma = memSys.dma;
    DelayMemTest dutMem = delayMem.to_test;


    rule doInitCoreTable(testFSM == InitTable && !coreTableInitDone);
        for(Integer i = 0; i < valueOf(L1DNum); i = i+1) begin
            dcReqTable[i].upd(iterId, Invalid);
            dcRespDoneTable[i].upd(iterId, False);
        end
        for(Integer i = 0; i < valueOf(L1INum); i = i+1) begin
            icReqTable[i].upd(iterId, Invalid);
            icRefTable[i].upd(iterId, Invalid);
        end
        // change state
        if(iterId == fromInteger(valueof(TestNum) - 1)) begin
            iterId <= 0;
            coreTableInitDone <= True;
        end
        else begin
            iterId <= iterId + 1;
        end
    endrule

    rule doInitDmaTable(testFSM == InitTable && !dmaTableInitDone);
        dmaReqTable.upd(iterDmaId, Invalid);
        dmaRespDoneTable.upd(iterDmaId, False);
        dmaRefWrMissTable.upd(iterDmaId, False);
        dmaRefWrHitTable.upd(iterDmaId, False);
        dmaRefRdMissTable.upd(iterDmaId, Invalid);
        dmaRefRdHitTable.upd(iterDmaId, Invalid);
        // change state
        if(iterDmaId == fromInteger(valueof(DmaTestNum) - 1)) begin
            iterDmaId <= 0;
            dmaTableInitDone <= True;
        end
        else begin
            iterDmaId <= iterDmaId + 1;
        end
    endrule

    rule doInitTableDone(testFSM == InitTable && coreTableInitDone && dmaTableInitDone);
        testFSM <= InitAddr;
        $fdisplay(stderr, "INFO: init table done");
    endrule

    rule doInitAddr(testFSM == InitAddr);
        Addr addr = getAddr(iterTag, iterIndex, 0);
        Line initV = replicate(addr);
        dutMem.initLine(addr, initV);
        refMem.writeLine(addr, initV);
        if(iterIndex == fromInteger(valueOf(IndexNum) - 1)) begin
            iterIndex <= 0;
            if(iterTag == fromInteger(valueOf(TagNum) - 1)) begin
                iterTag <= 0;
                // init randomizers and files for each core
                for(Integer i = 0; i < valueOf(L1DNum); i = i+1) begin
                    randDCReqStall[i].cntrl.init;
                    randDCOp[i].cntrl.init;
                    randDCTag[i].cntrl.init;
                    randDCIndex[i].cntrl.init;
                    randDCDataSel[i].cntrl.init;
                    randDCDataBE[i].cntrl.init;
                    randDCData[i].cntrl.init;
                    randDCLineBE[i].cntrl.init;
                    randDCLine[i].cntrl.init;
                    randDCAmoFunc[i].cntrl.init;
                    randDCDoubleWord[i].cntrl.init;
                    String name = sprintf("req_dc_%d.log", i);
                    File f <- $fopen(name, "w");
                    dcReqLog[i] <= f;
                    name = sprintf("resp_dc_%d.log", i);
                    f <- $fopen(name, "w");
                    dcRespLog[i] <= f;
                end
                for(Integer i = 0; i < valueOf(L1INum); i = i+1) begin
                    randICReqStall[i].cntrl.init;
                    randICTag[i].cntrl.init;
                    randICIndex[i].cntrl.init;
                    randICInstSel[i].cntrl.init;
                    String name = sprintf("req_ic_%d.log", i);
                    File f <- $fopen(name, "w");
                    icReqLog[i] <= f;
                    name = sprintf("resp_ic_%d.log", i);
                    f <- $fopen(name, "w");
                    icRespLog[i] <= f;
                end
                // init randomizers and files for DMA
                randDmaReqStall.cntrl.init;
                randDmaWrite.cntrl.init;
                randDmaBE.cntrl.init;
                randDmaTag.cntrl.init;
                randDmaIndex.cntrl.init;
                randDmaData.cntrl.init;
                File dma_f <- $fopen("req_dma.log", "w");
                dmaReqLog <= dma_f;
                dma_f <- $fopen("resp_dma.log", "w");
                dmaRespLog <= dma_f;
                // notify memory that init done
                dutMem.initDone;
                // change state
                testFSM <= Idle;
                $fdisplay(stderr, "INFO: init addr done");
            end
            else begin
                iterTag <= iterTag + 1;
            end
        end
        else begin
            iterIndex <= iterIndex + 1;
        end
    endrule

    Reg#(Bit#(64)) waitCount <- mkReg(0);

    rule simplyWait(testFSM == Idle);
        // wait for LLC to init all BRAMs
        waitCount <= waitCount + 1;
        if(waitCount == fromInteger(valueOf(TExp#(LLIndexSz)))) begin
            $display("%t %m Start Issuing Requests only now!!!!", $time);
            $fdisplay(stderr, "INFO: start issue req");
            testFSM <= Process;
        end
    endrule

    for(Integer i = 0; i < valueOf(L1DNum); i = i+1) begin
        rule doDCReq(testFSM == Process && sendDCCnt[i] < fromInteger(valueOf(TestNum)));
            // randomize req
            let index <- randDCIndex[i].next;
            let tag <- randDCTag[i].next;
            let sel <- randDCDataSel[i].next;
            let addr = getAddr(tag, index, sel);
            let op <- randDCOp[i].next;
            let data <- randDCData[i].next;
            let rBE <- randDCDataBE[i].next;
            ByteEn be = unpack(rBE);
            let line <- randDCLine[i].next;
            let rlbe <- randDCLineBE[i].next;
            LineByteEn lineBE = unpack(rlbe);
            let doubleWord <- randDCDoubleWord[i].next;
            let amoFunc <- randDCAmoFunc[i].next;
            let req = MemTestReq {
                id: truncate(sendDCCnt[i]),
                op: op,
                addr: addr,
                byteEn: be,
                data: data,
                lineBE: lineBE,
                line: line,
                amoInst: AmoInst {
                    func: amoFunc,
                    doubleWord: doubleWord,
                    aq: False,
                    rl: False
                }
            };
            // randomize stall
            let rStall <- randDCReqStall[i].next;
            if(!getReqStall(rStall)) begin
                // no stall, send req & record
                ifcDC[i].req(ProcRq {
                    id: zeroExtend(req.id),
                    addr: req.addr,
                    toState: getToState(req.op),
                    op: getMemOp(req.op),
                    byteEn: req.byteEn,
                    data: req.data,
                    amoInst: req.amoInst
                });
                sendDCReq[i].wset(req);

                // output req cnt
                if((sendDCCnt[i] + 1) == sendPrintDCCnt[i]) begin
                    $fdisplay(stderr, "INFO: %t D$ %d send req %d/%d",
                        $time, i, sendDCCnt[i] + 1, valueOf(TestNum)
                    );
                    sendPrintDCCnt[i] <= sendPrintDCCnt[i] + fromInteger(valueOf(TestPrintNum));
                end
            end
        endrule
    end

    for(Integer i = 0; i < valueOf(L1INum); i = i+1) begin
        rule doICReq(testFSM == Process && sendICCnt[i] < fromInteger(valueOf(TestNum)));
            // randomize req
            let index <- randICIndex[i].next;
            let tag <- randICTag[i].next;
            let sel <- randICInstSel[i].next;
            let addr = getInstAddr(tag, index, sel);
            // randomize stall
            let rStall <- randICReqStall[i].next;
            if(!getReqStall(rStall)) begin
                // no stall, send req & record
                ifcIC[i].req.put(addr);
                sendICReq[i].wset(addr);

                // output req cnt
                if((sendICCnt[i] + 1) == sendPrintICCnt[i]) begin
                    $fdisplay(stderr, "INFO: %t I$ %d send req %d/%d",
                        $time, i, sendICCnt[i] + 1, valueOf(TestNum)
                    );
                    sendPrintICCnt[i] <= sendPrintICCnt[i] + fromInteger(valueOf(TestPrintNum));
                end
            end
        endrule

        rule doICResp(testFSM == Process && recvICCnt[i] < fromInteger(valueof(TestNum)));
            // get actual I$ in order resp
            let rs <- ifcIC[i].resp.get;
            recvICResp[i].wset(rs);
        endrule

        (* fire_when_enabled *)
        rule doICDone;
            // get signal when I$ req is done
            // only keep the id, ignore the cache line for now
            let r <- ifcIC[i].done.get;
            recvICDone[i].wset(truncate(r.id));
        endrule
    end

    rule doDmaReq(testFSM == Process && sendDmaCnt < fromInteger(valueOf(DmaTestNum)));
        // randomize req
        let index <- randDmaIndex.next;
        let tag <- randDmaTag.next;
        let addr = getAddr(tag, index, 0);
        Bool write <- randDmaWrite.next;
        let data <- randDmaData.next;
        let rBE <- randDmaBE.next;
        LineByteEn be = unpack(rBE);
        DmaRq#(DmaTestId) req = DmaRq {
            addr: addr,
            byteEn: write ? be : replicate(False),
            data: unpack(data),
            id: truncate(sendDmaCnt)
        };
        // randomize stall
        let rStall <- randDmaReqStall.next;
        if(!getDmaReqStall(rStall)) begin
            // no stall, send req & record
            ifcDma.memReq.enq(DmaRq {
                addr: req.addr,
                byteEn: req.byteEn,
                data: req.data,
                id: zeroExtend(req.id)
            });
            sendDmaReq.wset(req);

            // output req cnt
            if((sendDmaCnt + 1) == sendPrintDmaCnt) begin
                $fdisplay(stderr, "INFO: %t DMA send req %d/%d",
                    $time, sendDmaCnt + 1, valueOf(DmaTestNum)
                );
                sendPrintDmaCnt <= sendPrintDmaCnt + fromInteger(valueOf(DmaTestPrintNum));
            end
        end
    endrule

    // it's fine to delay DMA resp deq
    rule doDmaLdResp(testFSM == Process && recvDmaCnt < fromInteger(valueOf(DmaTestNum)));
        let rs <- toGet(ifcDma.respLd).get;
        recvDmaResp.wset(DmaRs {
            data: rs.data,
            id: truncate(rs.id)
        });
    endrule

    rule doDmaStResp(testFSM == Process && recvDmaCnt < fromInteger(valueOf(DmaTestNum)));
        let rs <- toGet(ifcDma.respSt).get;
        recvDmaResp.wset(DmaRs {
            data: ?,
            id: truncate(rs)
        });
    endrule

    (* fire_when_enabled *)
    rule doDmaWrMiss(testFSM == Process);
        let id <- ifcDma.wrMissResp.get;
        recvDmaWrMiss.wset(truncate(id));
    endrule

    (* fire_when_enabled *)
    rule doDmaWrHit(testFSM == Process);
        let id <- ifcDma.wrHitResp.get;
        recvDmaWrHit.wset(truncate(id));
    endrule

    (* fire_when_enabled *)
    rule doDmaRdMiss(testFSM == Process);
        let id <- ifcDma.rdMissResp.get;
        recvDmaRdMiss.wset(truncate(id));
    endrule

    (* fire_when_enabled *)
    rule doDmaRdHit(testFSM == Process);
        let id <- ifcDma.rdHitResp.get;
        recvDmaRdHit.wset(truncate(id));
    endrule

    function L1BankId getBankId(Addr a) = truncate(a >> valueof(LgLineSzBytes));

    (* fire_when_enabled *)
    rule doConProcess(testFSM == Process);
        // changes to make on refLink
        Vector#(L1DNum, Maybe#(LineAddr)) wrAddr = replicate(Invalid); // for clear others' links
        Vector#(L1DNum, Maybe#(LineAddr)) lrAddr = replicate(Invalid); // for clear own link
        Vector#(L1DNum, Maybe#(L1BankId)) scBank = replicate(Invalid); // for clear own link
        Vector#(2, Maybe#(LineAddr)) dmaWrAddr = replicate(Invalid); // for clear all links

        // handle D$ req/resp
        for(Integer i = 0; i < valueOf(L1DNum); i = i+1) begin
            // store req just sent
            if(sendDCReq[i].wget matches tagged Valid .req) begin
                sendDCCnt[i] <= sendDCCnt[i] + 1;
                // save req
                dcReqTable[i].upd(req.id, Valid (req));
                // write log
                $fwrite(dcReqLog[i], "time %t: ", $time, fshow(req), "\n\n");
            end
            // check resp just recv
            if(recvDCResp[i].wget matches tagged Valid .resp) begin
                recvDCCnt[i] <= recvDCCnt[i] + 1; // incr recv cnt
                // set resp as done
                if(!dcRespDoneTable[i].sub(resp.id)) begin
                    dcRespDoneTable[i].upd(resp.id, True);
                end
                else begin
                    $fdisplay(stderr, "[TbL1LL] ERROR: D$ %d resp %x duplicate", i, resp.id);
                    $finish;
                end
                // get req
                let r = dcReqTable[i].sub(resp.id);
                if(isValid(r)) begin
                    // good
                end
                else begin
                    $fdisplay(stderr, "[TbL1LL] ERROR: D$ %d resp %x does not have valid req", i, resp.id);
                    $finish;
                end
                let req = validValue(r);
                // check resp type
                if(getMemRespType(req.op) == resp.t) begin
                    // good
                end
                else begin
                    $fdisplay(stderr, "[TbL1LL] ERROR: D$ %d resp %x wrong type", i, resp.id);
                    $finish;
                end
                // some common info
                let bankId = getBankId(req.addr);
                let lineAddr = getLineAddr(req.addr);
                // check resp val and apply actions to ref mem
                if(req.op == Ld) begin
                    // load: check value
                    let refData <- refMem.readData(req.addr);
                    let dutData = resp.data;
                    $fwrite(dcRespLog[i], "time %t: resp %x Ld\n", $time, resp.id,
                        "ref data %x\n", refData,
                        "dut data %x\n\n", dutData
                    );
                    if(refData == dutData) begin
                        // good
                    end
                    else begin
                        $fwrite(stderr, "[TbL1LL] ERROR: D$ %d wrong Ld resp %x\n", i, resp.id);
                        $finish;
                    end
                    // stats
                    ldCnt[i][bankId] <= ldCnt[i][bankId] + 1;
                end
                else if(req.op == St) begin
                    // store: apply to ref mem
                    Line line <- refMem.readLine(req.addr);
                    Line newLine = getUpdatedLine(line, req.lineBE, req.line);
                    refMem.writeLine(req.addr, newLine);
                    // set addr for clearing others' link
                    wrAddr[i] = Valid (lineAddr);
                    $fwrite(dcRespLog[i], "time %t: resp %x St\n\n", $time, resp.id);
                    // stats
                    stCnt[i][bankId] <= stCnt[i][bankId] + 1;
                end
                else if(req.op == Lr) begin
                    // load reserve: check value
                    let refData <- refMem.readData(req.addr);
                    let dutData = resp.data;
                    $fwrite(dcRespLog[i], "time %t: resp %x Lr\n", $time, resp.id,
                        "ref data %x\n", refData,
                        "dut data %x\n\n", dutData
                    );
                    if(refData == dutData) begin
                        // good
                    end
                    else begin
                        $fwrite(stderr, "[TbL1LL] ERROR: D$ %d wrong Lr resp %x\n", i, resp.id);
                        $finish;
                    end
                    // record Lr addr for setting link addr
                    lrAddr[i] = Valid (lineAddr);
                    // stats
                    lrCnt[i][bankId] <= lrCnt[i][bankId] + 1;
                end
                else if(req.op == Sc) begin
                    // store cond: check value, figure out whether ref mem is success
                    //Bool refSucc = refLink[i][bankId] == Valid (lineAddr);
                    //Data refData = refSucc ? fromInteger(valueof(ScSuccVal)) : fromInteger(valueof(ScFailVal));
                    let dutData = resp.data;
                    // FIXME TODO I cannot make BSV compiles with linkAddr estimation, so assume LR/SC is correctly implemented
                    let refData = dutData;
                    $fwrite(dcRespLog[i], "time %t: resp %x Sc\n", $time, resp.id,
                        "ref data %x\n", refData,
                        "dut data %x\n\n", dutData
                    );
                    if(refData == dutData || dutData == fromInteger(valueof(ScFailVal))) begin
                        // good: it is fine for dut to have failed Sc
                    end
                    else begin
                        $fwrite(stderr, "[TbL1LL] ERROR: D$ %d wrong Sc resp %x\n", i, resp.id);
                        $finish;
                    end
                    // update mem & link addr
                    scBank[i] = Valid (bankId); // record sc bank for clearing own link
                    if(dutData == fromInteger(valueof(ScSuccVal))) begin
                        // write mem
                        Data data <- refMem.readData(req.addr);
                        Data newData = getUpdatedData(data, req.byteEn, req.data);
                        refMem.writeData(req.addr, newData);
                        // record write addr for clear other link
                        wrAddr[i] = Valid (lineAddr);
                    end
                    // stats
                    scCnt[i][bankId] <= scCnt[i][bankId] + 1;
                    if(dutData == fromInteger(valueof(ScSuccVal))) begin
                        scSuccCnt[i][bankId] <= scSuccCnt[i][bankId] + 1;
                    end
                    else begin
                        scFailCnt[i][bankId] <= scFailCnt[i][bankId] + 1;
                    end
                end
                else if(req.op == Amo) begin
                    // AMO: check value
                    Data dutData = resp.data;
                    Bool upper32 = req.addr[2] == 1;
                    Data data <- refMem.readData(req.addr);
                    Data refData = req.amoInst.doubleWord ? data : signExtend(
                        upper32 ? data[63:32] : data[31:0]
                    );
                    $fwrite(dcRespLog[i], "time %t: resp %x Amo\n", $time, resp.id,
                        "ref data %x\n", refData,
                        "dut data %x\n\n", dutData
                    );
                    if(refData == dutData) begin
                        // good
                    end
                    else begin
                        $fwrite(stderr, "[TbL1LL] ERROR: D$ %d wrong Amo resp %x\n", i, resp.id);
                        $finish;
                    end
                    // update mem
                    Data newData = amoExec(req.amoInst, data, req.data, upper32);
                    refMem.writeData(req.addr, newData);
                    // record write addr for clear other link
                    wrAddr[i] = Valid (lineAddr);
                    // stats
                    amoCnt[i][bankId] <= amoCnt[i][bankId] + 1;
                end
`ifdef STORE_PREFETCH
                else if(req.op == StPrefetch) begin
                    // Store prefetch
                    $fwrite(dcRespLog[i], "time %t: resp %x StPrefetch\n\n", $time, resp.id);
                    // stats
                    stPrefetchCnt[i][bankId] <= stPrefetchCnt[i][bankId] + 1;
                end
`endif
                else begin
                    $fdisplay(stderr, "[TbL1LL] ERROR: D$ %d resp %x unknown op\n", i, resp.id);
                    $finish;
                end
                // reset time out
                dcTimeOut[i] <= 0;
            end
            else if(recvDCCnt[i] < sendDCCnt[i]) begin
                // incr time out
                dcTimeOut[i] <= dcTimeOut[i] + 1;
                if(dcTimeOut[i] >= fromInteger(valueOf(MaxTimeOut) - 1)) begin
                    $fwrite(stderr, "[TbL1LL] ERROR: D$ %d deadlock\n", i);
                    $finish;
                end
            end
        end

        // handle I$ req/resp
        for(Integer i = 0; i < valueOf(L1INum); i = i+1) begin
            // store req just sent
            if(sendICReq[i].wget matches tagged Valid .req) begin
                TestId id = truncate(sendICCnt[i]);
                sendICCnt[i] <= sendICCnt[i] + 1;
                // save req
                icReqTable[i].upd(id, Valid (req));
                // write log
                $fwrite(icReqLog[i], "time %t: ", $time, fshow(id), " ; ", fshow(req), "\n\n");
            end
            // apply I$ req done (always comes at least 1 cycle earlier than real resp)
            if(recvICDone[i].wget matches tagged Valid .id) begin
                if(!isValid(icRefTable[i].sub(id))) begin
                    // get req addr
                    let r = icReqTable[i].sub(id);
                    if(isValid(r)) begin
                        // good
                    end
                    else begin
                        $fdisplay(stderr, "[TbL1LL] ERROR: I$ %d done %x does not have valid req", i, id);
                        $finish;
                    end
                    Addr addr = validValue(r);
                    // get reference inst result
                    Line line <- refMem.readLine(addr);
                    Vector#(LineSzInst, Instruction) instVec = unpack(pack(line));
                    LineInstOffset baseSel = getLineInstOffset(addr);
                    Bool stop = False;
                    L1InstResult res = replicate(Invalid);
                    for(Integer j = 0; j < valueof(L1ISupSz); j = j+1) begin
                        if(!stop) begin
                            let sel = baseSel + fromInteger(j);
                            res[j] = Valid (instVec[sel]);
                            stop = sel == maxBound;
                        end
                    end
                    // record inst result
                    icRefTable[i].upd(id, Valid (res));
                    $fwrite(icRespLog[i], "time %t: done %x result ", $time, id, fshow(res), "\n\n");
                end
                else begin
                    $fdisplay(stderr, "[TbL1LL] ERROR: I$ %d done %x duplicate", i, id);
                    $finish;
                end
            end
            // check resp
            if(recvICResp[i].wget matches tagged Valid .dutResp) begin
                // resp is in order
                TestId id = truncate(recvICCnt[i]);
                recvICCnt[i] <= recvICCnt[i] + 1; // incr recv cnt
                // get reference result
                let r = icRefTable[i].sub(id);
                $fwrite(icRespLog[i], "time %t: resp %x\n", $time, id,
                    "ref inst ", fshow(r), "\n",
                    "dut inst ", fshow(dutResp), "\n\n"
                );
                if(isValid(r)) begin
                    // good
                end
                else begin
                    $fdisplay(stderr, "[TbL1LL] ERROR: I$ %d resp %x haven't done yet", i, id);
                    $finish;
                end
                let refResp = validValue(r);
                // check value
                if(refResp == dutResp) begin
                    // good
                end
                else begin
                    $fdisplay(stderr, "[TbL1LL] ERROR: I$ %d resp %x wrong inst", i, id);
                    $finish;
                end
                // reset timeout
                icTimeOut[i] <= 0;
            end
            else if(recvICCnt[i] < sendICCnt[i]) begin
                // incr time out
                icTimeOut[i] <= icTimeOut[i] + 1;
                if(icTimeOut[i] >= fromInteger(valueOf(MaxTimeOut) - 1)) begin
                    $fwrite(stderr, "[TbL1LL] ERROR: I$ %d deadlock\n", i);
                    $finish;
                end
            end
        end

        // handle DMA req/resp
        // store DMA req just sent
        if(sendDmaReq.wget matches tagged Valid .req) begin
            sendDmaCnt <= sendDmaCnt + 1;
            // save req
            dmaReqTable.upd(req.id, Valid (req));
            // write log
            $fwrite(dmaReqLog, "time %t: ", $time, fshow(req), "\n\n");
        end
        // apply DMA hit/miss resp (the signal of req taking effects)
        // XXX these resp always comes earlier than actual resp due to logic in LLBank.bsv
        if(recvDmaWrMiss.wget matches tagged Valid .dmaId) begin
            if(!dmaRefWrMissTable.sub(dmaId)) begin
                // get req
                let r = dmaReqTable.sub(dmaId);
                if(isValid(r)) begin
                    // good
                end
                else begin
                    $fdisplay(stderr, "[TbL1LL] ERROR: DMA wr miss %x does not have valid req", dmaId);
                    $finish;
                end
                let req = validValue(r);
                // update ref mem
                Line line <- refMem.readLine(req.addr);
                Line newLine = getUpdatedLine(line, req.byteEn, req.data);
                refMem.writeLine(req.addr, newLine);
                // record write addr for clear all link
                dmaWrAddr[0] = Valid (getLineAddr(req.addr));
                // record write miss
                dmaRefWrMissTable.upd(dmaId, True);
                $fwrite(dmaRespLog, "time %t: wr miss %x\n\n", $time, dmaId);
            end
            else begin
                $fdisplay(stderr, "[TbL1LL] ERROR: DMA wr miss %x duplicate", dmaId);
                $finish;
            end
        end
        if(recvDmaWrHit.wget matches tagged Valid .dmaId) begin
            if(!dmaRefWrHitTable.sub(dmaId)) begin
                // get req
                let r = dmaReqTable.sub(dmaId);
                if(isValid(r)) begin
                    // good
                end
                else begin
                    $fdisplay(stderr, "[TbL1LL] ERROR: DMA wr hit %x does not have valid req", dmaId);
                    $finish;
                end
                let req = validValue(r);
                // update ref mem
                Line line <- refMem.readLine(req.addr);
                Line newLine = getUpdatedLine(line, req.byteEn, req.data);
                refMem.writeLine(req.addr, newLine);
                // record write addr for clear all link
                dmaWrAddr[1] = Valid (getLineAddr(req.addr));
                // record write hit
                dmaRefWrHitTable.upd(dmaId, True);
                $fwrite(dmaRespLog, "time %t: wr hit %x\n\n", $time, dmaId);
            end
            else begin
                $fdisplay(stderr, "[TbL1LL] ERROR: DMA wr hit %x duplicate", dmaId);
                $finish;
            end
        end
        if(recvDmaRdMiss.wget matches tagged Valid .dmaId) begin
            if(!isValid(dmaRefRdMissTable.sub(dmaId))) begin
                let r = dmaReqTable.sub(dmaId);
                if(isValid(r)) begin
                    // good
                end
                else begin
                    $fdisplay(stderr, "[TbL1LL] ERROR: DMA rd %x does not have valid req", dmaId);
                    $finish;
                end
                let req = validValue(r);
                Line line <- refMem.readLine(req.addr);
                dmaRefRdMissTable.upd(dmaId, Valid (line));
                $fwrite(dmaRespLog, "time %t: rd miss %x\n\n", $time, dmaId);
            end
            else begin
                $fdisplay(stderr, "[TbL1LL] ERROR: DMA rd miss %x duplicate", dmaId);
                $finish;
            end
        end
        if(recvDmaRdHit.wget matches tagged Valid .dmaId) begin
            if(!isValid(dmaRefRdHitTable.sub(dmaId))) begin
                let r = dmaReqTable.sub(dmaId);
                if(isValid(r)) begin
                    // good
                end
                else begin
                    $fdisplay(stderr, "[TbL1LL] ERROR: DMA rd hit %x does not have valid req", dmaId);
                    $finish;
                end
                let req = validValue(r);
                Line line <- refMem.readLine(req.addr);
                dmaRefRdHitTable.upd(dmaId, Valid (line));
                $fwrite(dmaRespLog, "time %t: rd hit %x\n\n", $time, dmaId);
            end
            else begin
                $fdisplay(stderr, "[TbL1LL] ERROR: DMA rd hit %x duplicate", dmaId);
                $finish;
            end
        end
        // check actual DMA resp
        if(recvDmaResp.wget matches tagged Valid .resp) begin
            recvDmaCnt <= recvDmaCnt + 1; // incr recv cnt
            // set resp as done
            if(!dmaRespDoneTable.sub(resp.id)) begin
                dmaRespDoneTable.upd(resp.id, True);
            end
            else begin
                $fdisplay(stderr, "[TbL1LL] ERROR: DMA resp %x duplicate", resp.id);
                $finish;
            end
            // get req
            let r = dmaReqTable.sub(resp.id);
            if(isValid(r)) begin
                // good
            end
            else begin
                $fdisplay(stderr, "[TbL1LL] ERROR: DMA resp %x does not have valid req", resp.id);
                $finish;
            end
            let req = validValue(r);
            if(req.byteEn == replicate(False)) begin // read resp
                // get ref value
                Maybe#(Line) missResp = dmaRefRdMissTable.sub(resp.id);
                Maybe#(Line) hitResp = dmaRefRdHitTable.sub(resp.id);
                $fwrite(dmaRespLog, "time %t: resp %x Rd\n", $time,
                    "miss ref: ", fshow(missResp), "\n",
                    "hit ref: ", fshow(hitResp), "\n",
                    "dut resp: ", fshow(resp.data), "\n\n"
                );
                Line refResp = ?;
                if(isValid(hitResp) && !isValid(missResp)) begin
                    refResp = validValue(hitResp);
                end
                else if(isValid(missResp) && !isValid(hitResp)) begin
                    refResp = validValue(missResp);
                end
                else if(isValid(missResp) && isValid(hitResp)) begin
                    $fdisplay(stderr, "[TbL1LL] ERROR: DMA resp %x, both hitResp and missResp are valid", resp.id);
                    $finish;
                end
                else begin
                    $fdisplay(stderr, "[TbL1LL] ERROR: DMA resp %x, both hitResp and missResp are invalid", resp.id);
                    $finish;
                end
                // check against ref value
                if(resp.data == refResp) begin
                    // good
                end
                else begin
                    $fwrite(stderr, "[TbL1LL] ERROR: DMA wrong Rd resp\n");
                    $finish;
                end
            end
            else begin // write resp
                // check whether wr miss/hit has happened
                Bool wrMiss = dmaRefWrMissTable.sub(resp.id);
                Bool wrHit = dmaRefWrHitTable.sub(resp.id);
                $fwrite(dmaRespLog, "time %t: resp %x Wr, miss %b, hit %b\n\n", $time, resp.id, wrMiss, wrHit);
                if(wrMiss && !wrHit || !wrMiss && wrHit) begin
                    // good
                end
                else if(wrMiss && wrHit) begin
                    $fdisplay(stderr, "[TbL1LL] ERROR: time %t, DMA resp %x, both wrMiss and wrHit are valid",
                        $time, resp.id
                    );
                    $finish;
                end
                else begin
                    $fdisplay(stderr, "[TbL1LL] ERROR: time %t, DMA resp %x, both wrMiss and wrHit are invalid",
                        $time, resp.id
                    );
                    $finish;
                end
            end
            // reset time out
            dmaTimeOut <= 0;
        end
        else if(recvDmaCnt < sendDmaCnt) begin
            // incr time out
            dmaTimeOut <= dmaTimeOut + 1;
            if(dmaTimeOut >= fromInteger(valueOf(MaxTimeOut) - 1)) begin
                $fwrite(stderr, "[TbL1LL] ERROR: DMA deadlock\n");
                $finish(0);
            end
        end

        // TODO FIXME this doesn't finish compilation!!
        // update link addr
        //Vector#(L1DNum, Vector#(L1BankNum, Maybe#(LineAddr))) nextLink = refLink;
        //// reset by D$ write
        //for(Integer i = 0; i < valueof(L1DNum); i = i+1) begin
        //    if(wrAddr[i] matches tagged Valid .wa) begin
        //        let bid = getBankId({wa, 0});
        //        for(Integer j = 0; j < valueof(L1DNum); j = j+1) begin
        //            if(j != i && refLink[j][bid] == Valid (wa)) begin // will not reset its own link
        //                nextLink[j][bid] = Invalid;
        //            end
        //        end
        //    end
        //end
        //// reset by DMA write
        //for(Integer i = 0; i < 2; i = i+1) begin
        //    if(dmaWrAddr[i] matches tagged Valid .wa) begin
        //        let bid = getBankId({wa, 0});
        //        for(Integer j = 0; j < valueof(L1DNum); j = j+1) begin
        //            if(refLink[j][bid] == Valid (wa)) begin
        //                nextLink[j][bid] = Invalid;
        //            end
        //        end
        //    end
        //end
        //// reset by Sc
        //for(Integer i = 0; i < valueof(L1DNum); i = i+1) begin
        //    if(scBank[i] matches tagged Valid .bid) begin
        //        nextLink[i][bid] = Invalid;
        //    end
        //end
        //// set by Lr
        //for(Integer i = 0; i < valueof(L1DNum); i = i+1) begin
        //    if(lrAddr[i] matches tagged Valid .la) begin
        //        let bid = getBankId({la, 0});
        //        nextLink[i][bid] = Valid (la);
        //    end
        //end
        //refLink <= nextLink;
        
        // change state
        Bool done = True;
        for(Integer i = 0; i < valueOf(L1DNum); i = i+1) begin
            if(recvDCCnt[i] < fromInteger(valueOf(TestNum))) begin
                done = False;
            end
        end
        for(Integer i = 0; i < valueOf(L1INum); i = i+1) begin
            if(recvICCnt[i] < fromInteger(valueOf(TestNum))) begin
                done = False;
            end
        end
        if(recvDmaCnt < fromInteger(valueof(DmaTestNum))) begin
            done = False;
        end
        if(done) begin
            testFSM <= Done;
        end

        // set fired
        processFired.send;
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule checkProcessFire(testFSM == Process);
        if(!processFired) begin
            $fwrite(stderr, "[TbL1LL] ERROR: time %t, process cononicalize rule does not fire\n", $time);
            $finish;
        end
    endrule

    rule printStats(testFSM == Done);
        $fdisplay(stderr, "INFO: PASS");
        for(Integer i = 0; i < valueof(L1DNum); i = i+1) begin
            for(Integer j = 0; j < valueof(L1BankNum); j = j+1) begin
                $fdisplay(stderr, "STATS: D$ %d bank %d: Ld %d, St %d, Amo %d, Lr %d, Sc %d, Sc succ %d, Sc fail %d",
                    i, j, ldCnt[i][j], stCnt[i][j], amoCnt[i][j], lrCnt[i][j], scCnt[i][j], scSuccCnt[i][j], scFailCnt[i][j]
`ifdef STORE_PREFETCH
                    , ", St prefetch %d", stPrefetchCnt[i][j]
`endif
                );
            end
        end
        $finish;
    endrule
endmodule

