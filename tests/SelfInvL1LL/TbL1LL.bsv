
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
import DelayMemTypes::*;
import IdealDelayMem::*;
import Printf::*;
import ConfigReg::*;

import SelfInvL1LL::*;

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
typedef enum {Ld, St, Lr, Sc, Amo} MemTestOp deriving(Bits, Eq, FShow, Bounded);

function MemOp getMemOp(MemTestOp op);
    case(op)
        Ld: return Ld;
        St: return St;
        Lr: return Lr;
        Sc: return Sc;
        Amo: return Amo;
        default: return ?;
    endcase
endfunction

function Msi getToState(MemTestOp op);
    case(op)
        Ld: return S;
        Lr: return E;
        St, Sc, Amo: return M;
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

typedef enum {Ld, St, LrScAmo} MemRespType deriving(Bits, Eq, FShow);
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

// random execute reconcile fence (1/16)
typedef Bit#(4) RandReconcile;

function Bool getReconcile(RandReconcile x);
    return x == 0;
endfunction

// test FSM
typedef enum {InitTable, InitAddr, Idle, Process, Done} TestFSM deriving(Bits, Eq, FShow);

// reference WMM model
import "BDPI" function Action wmmInit(Bit#(8) core_num, Bit#(32) index_num, Bit#(32) tag_num);
import "BDPI" function ActionValue#(Bit#(8)) wmmFindLine(Bit#(8) core, Bit#(32) index, Bit#(32) tag, Bit#(512) line);
import "BDPI" function ActionValue#(Bit#(8)) wmmFindData(Bit#(8) core, Bit#(32) index, Bit#(32) tag, Bit#(8) sel, Bit#(64) data);
import "BDPI" function ActionValue#(Bit#(512)) wmmReadMemLine(Bit#(32) index, Bit#(32) tag);
import "BDPI" function ActionValue#(Bit#(64)) wmmReadMemData(Bit#(32) index, Bit#(32) tag, Bit#(8) sel);
import "BDPI" function Action wmmWriteMemLine(Bit#(32) index, Bit#(32) tag, Bit#(512) line);
import "BDPI" function Action wmmWriteMemData(Bit#(32) index, Bit#(32) tag, Bit#(8) sel, Bit#(64) data);
import "BDPI" function Action wmmClearAddr(Bit#(8) core, Bit#(32) index, Bit#(32) tag);
import "BDPI" function Action wmmReconcile(Bit#(8) core);
import "BDPI" function Action wmmPushStaleByCore(Bit#(8) core, Bit#(32) index, Bit#(32) tag);
import "BDPI" function Action wmmPushStaleByDma(Bit#(32) index, Bit#(32) tag);
import "BDPI" function Action wmmFinish();

interface RefMem;
    method ActionValue#(Bool) findLine(LLChild child, Addr a, Line line);
    method ActionValue#(Bool) findData(LLChild child, Addr a, Data data);
    method ActionValue#(Line) readMemLine(Addr a);
    method ActionValue#(Data) readMemData(Addr a);
    method Action writeMemLine(Addr a, Line line);
    method Action writeMemData(Addr a, Data data);
    method Action clearAddr(LLChild child, Addr a);
    method Action reconcile(LLChild child);
    method Action pushStaleByCore(LLChild child, Addr a);
    method Action pushStaleByDma(Addr a);
    method Action finish;
endinterface

module mkRefMem(RefMem) provisos(
    NumAlias#(LgLLBankNum, 0) // Only one LL bank
);
    Reg#(Bool) inited <- mkReg(False);

    rule doInit(!inited);
        wmmInit(fromInteger(valueof(L1Num)), fromInteger(valueof(IndexNum)), fromInteger(valueof(TagNum)));
        inited <= True;
    endrule

    function Bit#(32) getIndex(Addr a);
        LLCIndex idx = truncate(a >> valueof(LgLineSzBytes));
        return zeroExtend(idx);
    endfunction

    function Bit#(32) getTag(Addr a);
        LLCTag tag = truncateLSB(a);
        return truncate(tag);
    endfunction

    function Bit#(8) getDataSel(Addr a);
        return zeroExtend(getLineDataOffset(a));
    endfunction

    method ActionValue#(Bool) findLine(LLChild child, Addr a, Line line) if(inited);
        let r <- wmmFindLine(zeroExtend(child), getIndex(a), getTag(a), pack(line));
        return r == 1;
    endmethod

    method ActionValue#(Bool) findData(LLChild child, Addr a, Data data) if(inited);
        let r <- wmmFindData(zeroExtend(child), getIndex(a), getTag(a), getDataSel(a), data);
        return r == 1;
    endmethod

    method ActionValue#(Line) readMemLine(Addr a) if(inited);
        let line <- wmmReadMemLine(getIndex(a), getTag(a));
        return unpack(line);
    endmethod

    method ActionValue#(Data) readMemData(Addr a) if(inited);
        let data <- wmmReadMemData(getIndex(a), getTag(a), getDataSel(a));
        return data;
    endmethod

    method Action writeMemLine(Addr a, Line line) if(inited);
        wmmWriteMemLine(getIndex(a), getTag(a), pack(line));
    endmethod

    method Action writeMemData(Addr a, Data data) if(inited);
        wmmWriteMemData(getIndex(a), getTag(a), getDataSel(a), data);
    endmethod

    method Action clearAddr(LLChild child, Addr a) if(inited);
        wmmClearAddr(zeroExtend(child), getIndex(a), getTag(a));
    endmethod

    method Action reconcile(LLChild child) if(inited);
        wmmReconcile(zeroExtend(child));
    endmethod

    method Action pushStaleByCore(LLChild child, Addr a) if(inited);
        wmmPushStaleByCore(zeroExtend(child), getIndex(a), getTag(a));
    endmethod

    method Action pushStaleByDma(Addr a) if(inited);
        wmmPushStaleByDma(getIndex(a), getTag(a));
    endmethod

    method Action finish if(inited);
        wmmFinish;
    endmethod
endmodule

(* synthesize *)
module mkTbL1LL(Empty);
    // Reference
    RefMem refMem <- mkRefMem;
    //Reg#(Vector#(L1DNum, Vector#(L1BankNum, Maybe#(LineAddr)))) refLink <- mkReg(replicate(replicate(Invalid)));

    // randomize req
    // D$
    Vector#(L1DNum, Randomize#(ReqStall)) randDCReqStall <- replicateM(mkGenericRandomizer);
    Vector#(L1DNum, Randomize#(MemTestOp)) randDCOp <- replicateM(mkConstrainedRandomizer(Ld, Amo));
    Vector#(L1DNum, Randomize#(RandReconcile)) randDCReconcile <- replicateM(mkGenericRandomizer);
    Vector#(L1DNum, Randomize#(LLCTag)) randDCTag <- replicateM(mkConstrainedRandomizer(0, fromInteger(valueOf(TagNum) - 1)));
    Vector#(L1DNum, Randomize#(LLCIndex)) randDCIndex <- replicateM(mkConstrainedRandomizer(0, fromInteger(valueOf(IndexNum) - 1)));
    Vector#(L1DNum, Randomize#(LineDataOffset)) randDCDataSel <- replicateM(mkGenericRandomizer);
    Vector#(L1DNum, Randomize#(Data)) randDCData <- replicateM(mkGenericRandomizer);
    Vector#(L1DNum, Randomize#(Bit#(DataSzBytes))) randDCDataBE <- replicateM(mkConstrainedRandomizer(1, maxBound));
    Vector#(L1DNum, Randomize#(Line)) randDCLine <- replicateM(mkGenericRandomizer);
    Vector#(L1DNum, Randomize#(Bit#(LineSzBytes))) randDCLineBE <- replicateM(mkConstrainedRandomizer(1, maxBound));
    Vector#(L1DNum, Randomize#(AmoFunc)) randDCAmoFunc <- replicateM(mkConstrainedRandomizer(Swap, Maxu));
    Vector#(L1DNum, Randomize#(Bool)) randDCDoubleWord <- replicateM(mkGenericRandomizer);
    // I$
    Vector#(L1INum, Randomize#(ReqStall)) randICReqStall <- replicateM(mkGenericRandomizer);
    Vector#(L1DNum, Randomize#(RandReconcile)) randICReconcile <- replicateM(mkGenericRandomizer);
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
    Vector#(L1DNum, PulseWire) dcReqStall <- replicateM(mkPulseWire);
    Vector#(L1DNum, PulseWire) dcReqReconcile <- replicateM(mkPulseWire);
    Vector#(L1DNum, RegFile#(TestId, Maybe#(MemTestReq))) dcReqTable <- replicateM(mkRegFileFull);
    Vector#(L1DNum, Reg#(TestCnt)) sendDCCnt <- replicateM(mkReg(0));
    Vector#(L1DNum, RWire#(MemTestReq)) sendDCReq <- replicateM(mkRWire);
    Vector#(L1DNum, Reg#(Data)) dcStallCnt <- replicateM(mkReg(0));
    Vector#(L1DNum, Reg#(Data)) dcReconcileCnt <- replicateM(mkReg(0));
    Vector#(L1DNum, Reg#(Bool)) dcWaitReconcile <- replicateM(mkReg(False));
    // I$
    Vector#(L1INum, PulseWire) icReqStall <- replicateM(mkPulseWire);
    Vector#(L1INum, PulseWire) icReqReconcile <- replicateM(mkPulseWire);
    Vector#(L1INum, RegFile#(TestId, Maybe#(Addr))) icReqTable <- replicateM(mkRegFileFull);
    Vector#(L1INum, Reg#(TestCnt)) sendICCnt <- replicateM(mkReg(0));
    Vector#(L1INum, RWire#(Addr)) sendICReq <- replicateM(mkRWire);
    Vector#(L1INum, Reg#(Data)) icStallCnt <- replicateM(mkReg(0));
    Vector#(L1INum, Reg#(Data)) icReconcileCnt <- replicateM(mkReg(0));
    Vector#(L1INum, Reg#(Bool)) icWaitReconcile <- replicateM(mkReg(False));
    // DMA
    PulseWire dmaReqStall <- mkPulseWire;
    RegFile#(DmaTestId, Maybe#(DmaRq#(DmaTestId))) dmaReqTable <- mkRegFileFull;
    Reg#(DmaTestCnt) sendDmaCnt <- mkReg(0);
    RWire#(DmaRq#(DmaTestId)) sendDmaReq <- mkRWire;
    Reg#(Data) dmaStallCnt <- mkReg(0);

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
    Vector#(L1INum, RWire#(DebugICacheResp)) recvICDone <- replicateM(mkRWire);

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
    Vector#(L1DNum, Reg#(File)) dcReconcileLog <- replicateM(mkReg(InvalidFile));

    Vector#(L1INum, Reg#(File)) icReqLog <- replicateM(mkReg(InvalidFile));
    Vector#(L1INum, Reg#(File)) icRespLog <- replicateM(mkReg(InvalidFile));
    Vector#(L1INum, Reg#(File)) icReconcileLog <- replicateM(mkReg(InvalidFile));

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
            method Action evict(LineAddr a);
                noAction;
            endmethod
        endinterface);
    endfunction
    let memSys <- mkSelfInvL1LL(map(getL1ProcResp, genVector));
    IdealDelayMem#(MemDelay, LgTestMemSzBytes, LdMemRqId#(LLCRqMshrIdx), void) delayMem <- mkIdealDelayMem;
    mkConnection(memSys.to_mem, delayMem.to_proc);

    // XXX add 1 cycle delay to I$ resp, so done comes at least 1 cycle before resp
    Vector#(L1INum, FIFO#(L1InstResult)) icDelayQ <- replicateM(mkFIFO);
    for(Integer i = 0; i < valueof(L1INum); i = i+1) begin
        mkConnection(toPut(icDelayQ[i]), memSys.inst[i].resp);
    end

    // get all ifc used in the reset of the testbench
    Vector#(L1DNum, L1ProcReq#(ProcRqId)) ifcDC = memSys.dReq;
    Vector#(L1DNum, L1Reconcile) ifcDCReconcile = memSys.dReconcile;
    Vector#(L1INum, InstServer#(L1ISupSz)) ifcIC = ?;
    for(Integer i = 0; i < valueof(L1INum); i = i+1) begin
        ifcIC[i] = (interface InstServer;
            interface req = memSys.inst[i].req;
            interface resp = toGet(icDelayQ[i]); // use delayed resp
            interface done = memSys.inst[i].done;
        endinterface);
    end
    Vector#(L1INum, L1Reconcile) ifcICReconcile = memSys.iReconcile;
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
        refMem.writeMemLine(addr, initV);
        if(iterIndex == fromInteger(valueOf(IndexNum) - 1)) begin
            iterIndex <= 0;
            if(iterTag == fromInteger(valueOf(TagNum) - 1)) begin
                iterTag <= 0;
                // init randomizers and files for each core
                for(Integer i = 0; i < valueOf(L1DNum); i = i+1) begin
                    randDCReqStall[i].cntrl.init;
                    randDCOp[i].cntrl.init;
                    randDCReconcile[i].cntrl.init;
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
                    name = sprintf("reconcile_dc_%d.log", i);
                    f <- $fopen(name, "w");
                    dcReconcileLog[i] <= f;
                end
                for(Integer i = 0; i < valueOf(L1INum); i = i+1) begin
                    randICReqStall[i].cntrl.init;
                    randICReconcile[i].cntrl.init;
                    randICTag[i].cntrl.init;
                    randICIndex[i].cntrl.init;
                    randICInstSel[i].cntrl.init;
                    String name = sprintf("req_ic_%d.log", i);
                    File f <- $fopen(name, "w");
                    icReqLog[i] <= f;
                    name = sprintf("resp_ic_%d.log", i);
                    f <- $fopen(name, "w");
                    icRespLog[i] <= f;
                    name = sprintf("reconcile_ic_%d.log", i);
                    f <- $fopen(name, "w");
                    icReconcileLog[i] <= f;
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
        (* fire_when_enabled *)
        rule getRandDCReqStall;
            let r <- randDCReqStall[i].next;
            if(getReqStall(r)) begin
                dcReqStall[i].send;
            end
        endrule

        (* fire_when_enabled *)
        rule getRandDCReconcile;
            let r <- randDCReconcile[i].next;
            if(getReconcile(r)) begin
                dcReqReconcile[i].send;
            end
        endrule

        Bool canDCReq = (testFSM == Process && !dcWaitReconcile[i] &&
                         sendDCCnt[i] < fromInteger(valueOf(TestNum)));

        rule doDCReqStall(canDCReq && dcReqStall[i]);
            // stall
            dcStallCnt[i] <= dcStallCnt[i] + 1;
        endrule

        rule doDCReqReconcile(canDCReq && !dcReqStall[i] && dcReqReconcile[i]);
            // reconcile
            ifcDCReconcile[i].reconcile;
            dcWaitReconcile[i] <= True;
            dcReconcileCnt[i] <= dcReconcileCnt[i] + 1;
            $fwrite(dcReconcileLog[i], "time %t: send reconcile\n", $time);
        endrule

        rule doDCReq(canDCReq && !dcReqStall[i] && !dcReqReconcile[i]);
            // no stall or fence, send req & record
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
        endrule

        rule doDCReconcile(dcWaitReconcile[i] && ifcDCReconcile[i].reconcile_done);
            dcWaitReconcile[i] <= False;
            $fwrite(dcReconcileLog[i], "time %t: reconcile done\n", $time);
        endrule
    end

    for(Integer i = 0; i < valueOf(L1INum); i = i+1) begin
        (* fire_when_enabled *)
        rule getRandICReqStall;
            let r <- randICReqStall[i].next;
            if(getReqStall(r)) begin
                icReqStall[i].send;
            end
        endrule

        (* fire_when_enabled *)
        rule getRandICReconcile;
            let r <- randICReconcile[i].next;
            if(getReconcile(r)) begin
                icReqReconcile[i].send;
            end
        endrule

        Bool canICReq = (testFSM == Process && !icWaitReconcile[i] &&
                         sendICCnt[i] < fromInteger(valueOf(TestNum)));

        rule doICReqStall(canICReq && icReqStall[i]);
            // stall
            icStallCnt[i] <= icStallCnt[i] + 1;
        endrule

        rule doICReqReconcile(canICReq && !icReqStall[i] && icReqReconcile[i]);
            // reconcile
            ifcICReconcile[i].reconcile;
            icWaitReconcile[i] <= True;
            icReconcileCnt[i] <= icReconcileCnt[i] + 1;
            $fwrite(icReconcileLog[i], "time %t: send reconcile\n", $time);
        endrule

        rule doICReq(canICReq && !icReqStall[i] && !icReqReconcile[i]);
            // no stall or fence, send req & record
            // randomize req
            let index <- randICIndex[i].next;
            let tag <- randICTag[i].next;
            let sel <- randICInstSel[i].next;
            let addr = getInstAddr(tag, index, sel);
            ifcIC[i].req.put(addr);
            sendICReq[i].wset(addr);
            // output req cnt
            if((sendICCnt[i] + 1) == sendPrintICCnt[i]) begin
                $fdisplay(stderr, "INFO: %t I$ %d send req %d/%d",
                    $time, i, sendICCnt[i] + 1, valueOf(TestNum)
                );
                sendPrintICCnt[i] <= sendPrintICCnt[i] + fromInteger(valueOf(TestPrintNum));
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
            let r <- ifcIC[i].done.get;
            recvICDone[i].wset(r);
        endrule

        rule doICReconcile(icWaitReconcile[i] && ifcICReconcile[i].reconcile_done);
            icWaitReconcile[i] <= False;
            $fwrite(icReconcileLog[i], "time %t: reconcile done\n", $time);
        endrule
    end

    (* fire_when_enabled *)
    rule getRandDmaReqStall;
        let r <- randDmaReqStall.next;
        if(getDmaReqStall(r)) begin
            dmaReqStall.send;
        end
    endrule

    rule doDmaReq(testFSM == Process && sendDmaCnt < fromInteger(valueOf(DmaTestNum)));
        if(dmaReqStall) begin
            // stall
            dmaStallCnt <= dmaStallCnt + 1;
        end
        else begin
            // no stall, send req & record
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

    // DMA resp has been delayed after its taking-effect time in LLBank
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
        // changes to make on refLink (not really in use..)
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
                    let dutData = resp.data;
                    let found <- refMem.findData(fromInteger(getDChild(i)), req.addr, dutData);
                    $fwrite(dcRespLog[i], "time %t: resp %x Ld\n", $time, resp.id,
                            "dut data %x\n", dutData, "ref found ", fshow(found), "\n\n");
                    if(found) begin
                        // good
                    end
                    else begin
                        $fwrite(stderr, "[TbL1LL] ERROR: D$ %d fail to find Ld resp %x\n", i, resp.id);
                        $finish;
                    end
                    // stats
                    ldCnt[i][bankId] <= ldCnt[i][bankId] + 1;
                end
                else if(req.op == St) begin
                    // store: apply to ref mem
                    refMem.pushStaleByCore(fromInteger(getDChild(i)), req.addr);
                    refMem.clearAddr(fromInteger(getDChild(i)), req.addr);
                    Line line <- refMem.readMemLine(req.addr);
                    Line newLine = getUpdatedLine(line, req.lineBE, req.line);
                    refMem.writeMemLine(req.addr, newLine);
                    // set addr for clearing others' link
                    wrAddr[i] = Valid (lineAddr);
                    $fwrite(dcRespLog[i], "time %t: resp %x St\n\n", $time, resp.id);
                    // stats
                    stCnt[i][bankId] <= stCnt[i][bankId] + 1;
                end
                else if(req.op == Lr) begin
                    // load reserve: check value
                    let dutData = resp.data;
                    refMem.clearAddr(fromInteger(getDChild(i)), req.addr);
                    let refData <- refMem.readMemData(req.addr);
                    $fwrite(dcRespLog[i], "time %t: resp %x Lr\n", $time, resp.id,
                        "dut data %x\n", dutData,
                        "ref data %x\n\n", refData
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
                    $fwrite(dcRespLog[i], "time %t: resp %x Sc\n", $time, resp.id,
                        "dut data %x\n\n", dutData
                        //"ref data %x\n", refData
                    );
                    //if(refData == dutData || dutData == fromInteger(valueof(ScFailVal))) begin
                    //    // good: it is fine for dut to have failed Sc
                    //end
                    //else begin
                    //    $fwrite(stderr, "[TbL1LL] ERROR: D$ %d wrong Sc resp %x\n", i, resp.id);
                    //    $finish;
                    //end
                    // update mem & link addr
                    scBank[i] = Valid (bankId); // record sc bank for clearing own link
                    if(dutData == fromInteger(valueof(ScSuccVal))) begin
                        // write mem
                        refMem.pushStaleByCore(fromInteger(getDChild(i)), req.addr);
                        refMem.clearAddr(fromInteger(getDChild(i)), req.addr);
                        Data data <- refMem.readMemData(req.addr);
                        Data newData = getUpdatedData(data, req.byteEn, req.data);
                        refMem.writeMemData(req.addr, newData);
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
                    refMem.pushStaleByCore(fromInteger(getDChild(i)), req.addr);
                    refMem.clearAddr(fromInteger(getDChild(i)), req.addr);
                    Data data <- refMem.readMemData(req.addr);
                    Data refData = req.amoInst.doubleWord ? data : signExtend(
                        upper32 ? data[63:32] : data[31:0]
                    );
                    $fwrite(dcRespLog[i], "time %t: resp %x Amo\n", $time, resp.id,
                        "dut data %x\n", dutData,
                        "ref data %x\n\n", refData
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
                    refMem.writeMemData(req.addr, newData);
                    // record write addr for clear other link
                    wrAddr[i] = Valid (lineAddr);
                    // stats
                    amoCnt[i][bankId] <= amoCnt[i][bankId] + 1;
                end
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
            if(recvICDone[i].wget matches tagged Valid .resp) begin
                TestId id = truncate(resp.id);
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
                    Vector#(LineSzInst, Instruction) instVec = unpack(pack(resp.line));
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
                    // find in ref mem
                    let found <- refMem.findLine(fromInteger(getIChild(i)), addr, resp.line);
                    // record inst result
                    icRefTable[i].upd(id, Valid (res));
                    $fwrite(icRespLog[i], "time %t: done %x \n", $time, id,
                            "dut line ", fshow(resp.line), ", result ", fshow(res), "\n",
                            "ref found ", fshow(found), "\n\n");
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
                    "dut inst ", fshow(dutResp), "\n",
                    "done inst ", fshow(r), "\n\n"
                );
                if(isValid(r)) begin
                    // good
                end
                else begin
                    $fdisplay(stderr, "[TbL1LL] ERROR: I$ %d resp %x haven't done yet", i, id);
                    $finish;
                end
                let doneResp = validValue(r);
                // check value
                if(doneResp == dutResp) begin
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
                refMem.pushStaleByDma(req.addr);
                Line line <- refMem.readMemLine(req.addr);
                Line newLine = getUpdatedLine(line, req.byteEn, req.data);
                refMem.writeMemLine(req.addr, newLine);
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
                refMem.pushStaleByDma(req.addr);
                Line line <- refMem.readMemLine(req.addr);
                Line newLine = getUpdatedLine(line, req.byteEn, req.data);
                refMem.writeMemLine(req.addr, newLine);
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
                Line line <- refMem.readMemLine(req.addr);
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
                Line line <- refMem.readMemLine(req.addr);
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
                    "dut resp: ", fshow(resp.data), "\n",
                    "miss ref: ", fshow(missResp), "\n",
                    "hit ref: ", fshow(hitResp), "\n\n"
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
                );
            end
        end
        for(Integer i = 0; i < valueof(L1DNum); i = i+1) begin
            $fdisplay(stderr, "STATS: D$ %d: stall %d, reconcile %d", i, dcStallCnt[i], dcReconcileCnt[i]);
        end
        for(Integer i = 0; i < valueof(L1INum); i = i+1) begin
            $fdisplay(stderr, "STATS: I$ %d: stall %d, reconcile %d", i, icStallCnt[i], icReconcileCnt[i]);
        end
        $fdisplay(stderr, "STATS: DMA: stall %d", dmaStallCnt);
        refMem.finish;
        $finish;
    endrule
endmodule

