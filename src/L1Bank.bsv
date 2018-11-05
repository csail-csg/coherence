
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

`include "ProcConfig.bsv"

import Types::*;
import MemoryTypes::*;
import Amo::*;

import Cntrs::*;
import Vector::*;
import ConfigReg::*;
import BuildVector::*;
import FIFO::*;
import GetPut::*;
import CCTypes::*;
import L1CRqMshr::*;
import L1PRqMshr::*;
import CCPipe::*;
import L1Pipe ::*;
import FShow::*;
import DefaultValue::*;
import Ehr::*;
import Fifo::*;
import CacheUtils::*;
import CrossBar::*;
import Performance::*;
import LatencyTimer::*;

// although pRq never appears in dependency chain
// we still need pRq MSHR to limit the number of pRq
// and thus limit the size of rsToPIndexQ

typedef struct {
    Addr addr;
    MemOp op;
    L1CRqState state;
    Msi slotCs;
    Bool waitP;
} L1CRqStuck deriving(Bits, Eq, FShow);

typedef L1PRqMshrStuck L1PRqStuck;

interface L1Bank#(
    numeric type lgBankNum,
    numeric type wayNum,
    numeric type indexSz,
    numeric type tagSz,
    numeric type cRqNum,
    numeric type pRqNum,
    type procRqIdT // id of req from processor core
);
    interface ChildCacheToParent#(Bit#(TLog#(wayNum)), void) to_parent;
    interface L1ProcReq#(procRqIdT) procReq;
    // reset link addr
    method Action resetLinkAddr;
    // detect deadlock: only in use when macro CHECK_DEADLOCK is defined
    interface Get#(L1CRqStuck) cRqStuck;
    interface Get#(L1PRqStuck) pRqStuck;
    // security: flush
    method Action flush;
    method Bool flush_done;
    // performance
    method Action setPerfStatus(Bool stats);
    method Data getPerfData(L1DPerfType t);
endinterface

module mkL1Bank#(
    Bit#(lgBankNum) bankId,
    module#(L1CRqMshr#(cRqNum, wayT, tagT, procRqT)) mkL1CRqMshrLocal,
    module#(L1PRqMshr#(pRqNum)) mkL1PRqMshrLocal,
    module#(L1Pipe#(lgBankNum, wayNum, indexT, tagT, cRqIdxT, pRqIdxT)) mkL1Pipeline,
    L1ProcResp#(procRqIdT) procResp
)(
    L1Bank#(lgBankNum, wayNum, indexSz, tagSz, cRqNum, pRqNum, procRqIdT)
) provisos(
    Alias#(wayT, Bit#(TLog#(wayNum))),
    Alias#(indexT, Bit#(indexSz)),
    Alias#(tagT, Bit#(tagSz)),
    Alias#(cRqIdxT, Bit#(TLog#(cRqNum))),
    Alias#(pRqIdxT, Bit#(TLog#(pRqNum))),
    Alias#(cacheOwnerT, Maybe#(cRqIdxT)), // actually owner cannot be pRq
    Alias#(cacheInfoT, CacheInfo#(tagT, Msi, void, cacheOwnerT)),
    Alias#(ramDataT, RamData#(tagT, Msi, void, cacheOwnerT, Line)),
    Alias#(procRqT, ProcRq#(procRqIdT)),
    Alias#(cRqToPT, CRqMsg#(wayT, void)),
    Alias#(cRsToPT, CRsMsg#(void)),
    Alias#(pRqFromPT, PRqMsg#(void)),
    Alias#(pRsFromPT, PRsMsg#(wayT, void)),
    Alias#(pRqRsFromPT, PRqRsMsg#(wayT, void)),
    Alias#(cRqSlotT, L1CRqSlot#(wayT, tagT)), // cRq MSHR slot
    Alias#(l1CmdT, L1Cmd#(indexT, cRqIdxT, pRqIdxT)),
    Alias#(pipeOutT, PipeOut#(wayT, tagT, Msi, void, cacheOwnerT, Line, l1CmdT)),
    // requirements
    Bits#(procRqIdT, _procRqIdT),
    FShow#(procRqIdT),
    FShow#(pipeOutT),
    Add#(tagSz, a__, AddrSz),
    // make sure: cRqNum <= wayNum
    Add#(cRqNum, b__, wayNum),
    Add#(TAdd#(tagSz, indexSz), TAdd#(lgBankNum, LgLineSzBytes), AddrSz)
);

    L1CRqMshr#(cRqNum, wayT, tagT, procRqT) cRqMshr <- mkL1CRqMshrLocal;

    L1PRqMshr#(pRqNum) pRqMshr <- mkL1PRqMshrLocal;

    L1Pipe#(lgBankNum, wayNum, indexT, tagT, cRqIdxT, pRqIdxT) pipeline <- mkL1Pipeline;

    Fifo#(1, procRqT) rqFromCQ <- mkBypassFifo;

    Fifo#(2, cRsToPT) rsToPQ <- mkCFFifo;
    Fifo#(2, cRqToPT) rqToPQ <- mkCFFifo;
    Fifo#(2, pRqRsFromPT) fromPQ <- mkCFFifo;

    FIFO#(MshrIndex#(cRqIdxT, pRqIdxT)) rsToPIndexQ <- mkSizedFIFO(valueOf(TAdd#(cRqNum, pRqNum)));

    FIFO#(cRqIdxT) rqToPIndexQ <- mkSizedFIFO(valueOf(cRqNum));
    // temp fifo for pipelineResp & sendRsToP (reduce conflict)
    FIFO#(cRqIdxT) rqToPIndexQ_pipelineResp <- mkFIFO;
    FIFO#(cRqIdxT) rqToPIndexQ_sendRsToP <- mkFIFO;

    Ehr#(2, Maybe#(LineAddr)) linkAddrEhr <- mkEhr(Invalid); // load reservation
    Reg#(Maybe#(LineAddr)) linkAddr = linkAddrEhr[0]; // normal processing use port 0
    Reg#(Maybe#(LineAddr)) linkAddrRst = linkAddrEhr[1]; // reset by outside use port 1

    // we process AMO resp in a new cycle to cut critical path
    Reg#(Maybe#(Tuple2#(cRqIdxT, procRqT))) processAmo <- mkReg(Invalid);

    // security flush
`ifdef SECURITY
    Reg#(Bool) flushDone <- mkReg(True);
    Reg#(Bool) flushReqStart <- mkReg(False);
    Reg#(Bool) flushReqDone <- mkReg(False);
    Reg#(Bool) flushRespDone <- mkReg(False);
    Reg#(indexT) flushIndex <- mkReg(0);
    Reg#(wayT) flushWay <- mkReg(0);
`else
    Bool flushDone = True;
`endif

    // performance
`ifdef PERF_COUNT
    Reg#(Bool) doStats <- mkConfigReg(False);
    Count#(Data) ldCnt <- mkCount(0);
    Count#(Data) stCnt <- mkCount(0);
    Count#(Data) amoCnt <- mkCount(0);
    Count#(Data) ldMissCnt <- mkCount(0);
    Count#(Data) stMissCnt <- mkCount(0);
    Count#(Data) amoMissCnt <- mkCount(0);
    Count#(Data) ldMissLat <- mkCount(0);
    Count#(Data) stMissLat <- mkCount(0);
    Count#(Data) amoMissLat <- mkCount(0);
    
    LatencyTimer#(cRqNum, 10) latTimer <- mkLatencyTimer; // max 1K cycle latency

    function Action incrReqCnt(MemOp op);
    action
        if(doStats) begin
            case(op)
                Ld: ldCnt.incr(1);
                St: stCnt.incr(1);
                Lr, Sc, Amo: amoCnt.incr(1);
            endcase
        end
    endaction
    endfunction

    function Action incrMissCnt(MemOp op, cRqIdxT idx);
    action
        let lat <- latTimer.done(idx);
        if(doStats) begin
            case(op)
                Ld: begin
                    ldMissLat.incr(zeroExtend(lat));
                    ldMissCnt.incr(1);
                end
                St: begin
                    stMissLat.incr(zeroExtend(lat));
                    stMissCnt.incr(1);
                end
                Lr, Sc, Amo: begin
                    amoMissLat.incr(zeroExtend(lat));
                    amoMissCnt.incr(1);
                end
            endcase
        end
    endaction
    endfunction
`endif

    function tagT getTag(Addr a) = truncateLSB(a);

    // although D$ may not have cRq at every cycle
    // we still make cRq has lower priorty than pRq/pRs
    // we stop accepting cRq when we need to flush for security
    rule cRqTransfer(flushDone);
        procRqT r <- toGet(rqFromCQ).get;
        cRqIdxT n <- cRqMshr.getEmptyEntryInit(r);
        // send to pipeline
        pipeline.send(CRq (L1PipeRqIn {
            addr: r.addr, 
            mshrIdx: n
        }));
`ifdef PERF_COUNT
        // performance counter: cRq type
        incrReqCnt(r.op);
`endif
        $display("%t L1 %m cRqTransfer: ", $time, 
            fshow(n), " ; ",
            fshow(r)
        );
    endrule

    (* descending_urgency = "pRqTransfer, cRqTransfer" *)
    rule pRqTransfer(fromPQ.first matches tagged PRq .req);
        fromPQ.deq;
        pRqIdxT n <- pRqMshr.getEmptyEntryInit(req);
        // send to pipeline
        pipeline.send(PRq (L1PipeRqIn {
            addr: req.addr,
            mshrIdx: n
        }));
        $display("%t L1 %m pRqTransfer: ", $time, 
            fshow(n), " ; ", 
            fshow(req)
        );
    endrule

    (* descending_urgency = "pRsTransfer, cRqTransfer" *)
    rule pRsTransfer(fromPQ.first matches tagged PRs .resp);
        fromPQ.deq;
        pipeline.send(PRs (L1PipePRsIn {
            addr: resp.addr,
            toState: resp.toState,
            data: resp.data,
            way: resp.id
        }));
        $display("%t L1 %m pRsTransfer: ", $time, fshow(resp));
    endrule

`ifdef SECURITY
    // start flush when cRq MSHR is empty
    rule startFlushReq(!flushDone && !flushReqStart && cRqMshr.emptyForFlush);
        flushReqStart <= True;
    endrule

    (* descending_urgency = "pRsTransfer, flushTransfer" *)
    (* descending_urgency = "pRqTransfer, flushTransfer" *)
    rule flushTransfer(!flushDone && flushReqStart && !flushReqDone);
        // We allocate a pRq MSHR entry for 2 reasons:
        // (1) reuse the pRq logic to send resp to parent
        // (2) control the number of downgrade resp to avoid stalling the cache
        // pipeline
        pRqIdxT n <- pRqMshr.getEmptyEntryInit(PRqMsg {
            addr: ?,
            toState: I,
            child: ?
        });
        pipeline.send(Flush (L1PipeFlushIn {
            index: flushIndex,
            way: flushWay,
            mshrIdx: n
        }));
        // increment flush index/way
        if (flushWay < fromInteger(valueof(wayNum) - 1)) begin
            flushWay <= flushWay + 1;
        end
        else begin
            flushWay <= 0;
            flushIndex <= flushIndex + 1; // index num should be power of 2
            if (flushIndex == maxBound) begin
                flushReqDone <= True;
            end
        end
        $display("%t L1 %m flushTransfer: ", $time, fshow(n), " ; ",
                 fshow(flushIndex), " ; ", fshow(flushWay));
    endrule
`endif

    rule sendRsToP_cRq(rsToPIndexQ.first matches tagged CRq .n);
        rsToPIndexQ.deq;
        // get cRq replacement info
        procRqT req = cRqMshr.sendRsToP_cRq.getRq(n);
        cRqSlotT slot = cRqMshr.sendRsToP_cRq.getSlot(n);
        Maybe#(Line) data = cRqMshr.sendRsToP_cRq.getData(n);
        L1CRqState state = cRqMshr.sendRsToP_cRq.getState(n);
        doAssert(state == WaitNewTag, 
            "send replacement resp to parent, state should be WaitNewTag" 
        );
        // send resp to parent
        cRsToPT resp = CRsMsg {
            addr: {slot.repTag, truncate(req.addr)}, // get bank id & index from req
            toState: I,
            data: data,
            child: ?
        };
        rsToPQ.enq(resp);
        // req parent for upgrade & change state
        rqToPIndexQ_sendRsToP.enq(n);
        cRqMshr.sendRsToP_cRq.setWaitSt_setSlot_clearData(n, L1CRqSlot {
            way: slot.way,
            cs: I, // replacement, so I (get ready for rqToIndex.deq)
            repTag: ?,
            waitP: True // we have req parent at the same time
        });
        // inform processor of line eviction
        procResp.evict(getLineAddr(resp.addr));
        $display("%t L1 %m sendRsToP: ", $time, 
            fshow(rsToPIndexQ.first)," ; ", 
            fshow(req), " ; ", 
            fshow(resp)
        );
    endrule

    rule sendRsToP_pRq(rsToPIndexQ.first matches tagged PRq .n);
        rsToPIndexQ.deq;
        // get pRq info & send resp & release MSHR entry
        pRqFromPT req = pRqMshr.sendRsToP_pRq.getRq(n);
        Maybe#(Line) data = pRqMshr.sendRsToP_pRq.getData(n);
        cRsToPT resp = CRsMsg {
            addr: req.addr,
            toState: req.toState,
            data: data,
            child: ?
        };
        rsToPQ.enq(resp);
        pRqMshr.sendRsToP_pRq.releaseEntry(n); // mshr entry released
        // inform processor of line eviction
        procResp.evict(getLineAddr(resp.addr));
        $display("%t L1 %m sendRsToP: ", $time, 
            fshow(rsToPIndexQ.first), " ; ", 
            fshow(req), " ; ", 
            fshow(resp)
        );
    endrule

    rule sendRqToP;
        rqToPIndexQ.deq;
        cRqIdxT n = rqToPIndexQ.first;
        procRqT req = cRqMshr.sendRqToP.getRq(n);
        cRqSlotT slot = cRqMshr.sendRqToP.getSlot(n);
        cRqToPT cRqToP = CRqMsg {
            addr: req.addr,
            fromState: slot.cs,
            toState: req.toState,
            id: slot.way,
            child: ?
        };
        rqToPQ.enq(cRqToP);
        $display("%t L1 %m sendRqToP: ", $time, 
            fshow(n), " ; ", 
            fshow(req), " ; ", 
            fshow(slot), " ; ", 
            fshow(cRqToP)
        );
`ifdef PERF_COUNT
        // performance counter: start miss timer
        latTimer.start(n);
`endif
    endrule

    // last stage of pipeline: process req

    // XXX: in L1, pRq cannot exist in dependency chain
    // because there are only two ways to include pRq into chain
    // (1) append to a cRq that could finish, but such cRq must have been directly reponded
    // (2) overtake cRq (S->M), but such downgrade can be done instaneously without the need of chaining
    // Thus, dependency chain in L1 only contains cRq

    // pipeline outputs
    pipeOutT pipeOut = pipeline.first;
    ramDataT ram = pipeOut.ram;
    // figure out procRq MSHR idx in pipeline output (since there is only one
    // port to select from MSHR)
    cRqIdxT pipeOutCRqIdx = (case(pipeOut.cmd) matches
        tagged L1CRq .n: (n);
        default: (fromMaybe(0, ram.info.owner)); // L1PRs and L1PRq
    endcase);
    procRqT pipeOutCRq = cRqMshr.pipelineResp.getRq(pipeOutCRqIdx);

    // function to process cRq hit (MSHR slot may have garbage)
    function Action cRqHit(cRqIdxT n, procRqT req);
    action
        $display("%t L1 %m pipelineResp: Hit func: ", $time,
            fshow(n), " ; ",
            fshow(req)
        );
        // check tag & cs: even this function is called by pRs, tag should match,
        // because tag is written into cache before sending req to parent
        doAssert(ram.info.tag == getTag(req.addr) && ram.info.cs >= req.toState,
            "cRqHit but tag or cs incorrect"
        );
        // process req: resp processor and get new cache line
        // TODO when we have MESI, cache state may also need update
        Line curLine = ram.line;
        Line newLine = curLine;
        LineDataOffset dataSel = getLineDataOffset(req.addr);
        case(req.op) matches
            Ld: begin
                procResp.respLd(req.id, curLine[dataSel]);
            end
            Lr: begin
                procResp.respLrScAmo(req.id, curLine[dataSel]);
                // set link addr
                linkAddr <= Valid (getLineAddr(req.addr));
            end
            Amo: begin
                noAction;
            end
            Sc: begin
                // check Sc succeeds or not
                Bool succeed = linkAddr == Valid (getLineAddr(req.addr));
                // resp to proc
                Data respVal = succeed ? fromInteger(valueof(ScSuccVal)) : fromInteger(valueof(ScFailVal));
                procResp.respLrScAmo(req.id, respVal);
                // calculate new data to write
                if(succeed) begin
                    newLine[dataSel] = getUpdatedData(curLine[dataSel], req.byteEn, req.data);
                end
                // reset link addr
                linkAddr <= Invalid;
            end
            St: begin
                // resp processor, get write data & BE
                let {be, wrLine} <- procResp.respSt(req.id);
                // calculate new data to write
                newLine = getUpdatedLine(curLine, be, wrLine);
            end
            default: begin
                doAssert(False, "unknown mem op");
            end
        endcase
        // deq pipeline or swap in successor ONLY when not AMO: to cut critical
        // path for AMO
        if(req.op != Amo) begin
            Maybe#(cRqIdxT) succ = cRqMshr.pipelineResp.getSucc(n);
            pipeline.deqWrite(succ, RamData {
                info: CacheInfo {
                    tag: getTag(req.addr), // should be the same as original tag
                    cs: ram.info.cs, // use cs in ram, because ram.info.cs > req.toState is possible
                    dir: ?,
                    owner: succ
                },
                line: newLine // write new data into cache
            });
            $display("%t L1 %m pipelineResp: Hit func: update ram: ", $time,
                fshow(newLine), " ; ",
                fshow(succ)
            );
            // release MSHR entry
            cRqMshr.pipelineResp.releaseEntry(n);
        end
        else begin
            processAmo <= Valid (tuple2(n, req));
            $display("%t L1 %m pipelineResp: Hit func: AMO process in next cycle", $time);
        end
    endaction
    endfunction

    rule doProcessAmo(processAmo matches tagged Valid {.n, .req});
        Line curLine = ram.line;
        Line newLine = curLine;
        LineDataOffset dataSel = getLineDataOffset(req.addr);
        Bool upper32 = req.addr[2] == 1;
        Data curData = curLine[dataSel];
        // resp processor
        Data resp = req.amoInst.doubleWord ? curData : signExtend(
            upper32 ? curData[63:32] : curData[31:0]
        );
        procResp.respLrScAmo(req.id, resp);
        // calculate new data to write
        newLine[dataSel] = amoExec(req.amoInst, curData, req.data, upper32);
        // deq pipeline or swap in successor
        Maybe#(cRqIdxT) succ = cRqMshr.pipelineResp.getSucc(n);
        pipeline.deqWrite(succ, RamData {
            info: CacheInfo {
                tag: getTag(req.addr), // should be the same as original tag
                cs: ram.info.cs, // use cs in ram, because ram.info.cs > req.toState is possible
                dir: ?,
                owner: succ
            },
            line: newLine // write new data into cache
        });
        $display("%t L1 %m processAmo: update ram: ", $time,
            fshow(newLine), " ; ",
            fshow(succ)
        );
        // release MSHR entry
        cRqMshr.pipelineResp.releaseEntry(n);
        // reset state
        processAmo <= Invalid;
    endrule

    rule pipelineResp_cRq(!isValid(processAmo) &&& pipeOut.cmd matches tagged L1CRq .n);
        $display("%t L1 %m pipelineResp: ", $time, fshow(pipeOut));

        procRqT procRq = pipeOutCRq;
        $display("%t L1 %m pipelineResp: cRq: ", $time, fshow(n), " ; ", fshow(procRq));
        
        // find end of dependency chain
        Maybe#(cRqIdxT) cRqEOC = cRqMshr.pipelineResp.searchEndOfChain(procRq.addr);

        // function to process cRq miss without replacement (MSHR slot may have garbage)
        function Action cRqMissNoReplacement;
        action
            cRqSlotT cSlot = cRqMshr.pipelineResp.getSlot(n);
            // it is impossible in L1 to have slot.waitP == True in this function
            // because cRq is not set to Depend when pRq invalidates it (pRq just directly resp)
            // and this func is only called when cs < toState (otherwise will hit)
            // because L1 has no children to wait for
            doAssert(!cSlot.waitP && ram.info.cs < procRq.toState, 
                "waitP must be false and cs must < toState"
            );
            // Thus we must send req to parent 
            // XXX first send to a temp indexQ to avoid conflict, then merge to rqToPIndexQ later
            rqToPIndexQ_pipelineResp.enq(n);
            // update mshr
            cRqMshr.pipelineResp.setStateSlot(n, WaitSt, L1CRqSlot {
                way: pipeOut.way, // use way from pipeline
                cs: ram.info.cs, // record cs for future rqToPIndexQ.deq
                repTag: ?, // no replacement
                waitP: ram.info.cs < procRq.toState
            });
            // deq pipeline & set owner, tag
            pipeline.deqWrite(Invalid, RamData {
                info: CacheInfo {
                    tag: getTag(procRq.addr), // tag may be garbage if cs == I
                    cs: ram.info.cs,
                    dir: ?,
                    owner: Valid (n) // owner is req itself
                },
                line: ram.line
            });
        endaction
        endfunction

        // function to do replacement for cRq
        function Action cRqReplacement;
        action
            // deq pipeline
            pipeline.deqWrite(Invalid, RamData {
                info: CacheInfo {
                    tag: getTag(procRq.addr), // set to req tag (old tag is replaced right now)
                    cs: I,
                    dir: ?,
                    owner: Valid (n) // owner is req itself
                },
                line: ? // data is no longer used
            });
            // update MSHR: may save replaced line data
            cRqMshr.pipelineResp.setStateSlot(n, WaitNewTag, L1CRqSlot {
                way: pipeOut.way, // use way from pipeline
                cs: I,
                repTag: ram.info.tag, // tag being replaced
                waitP: False // we send req to parent later (when resp to parent is sent)
            });
            cRqMshr.pipelineResp.setData(n, ram.info.cs == M ? Valid (ram.line) : Invalid);
            // send replacement resp to parent
            rsToPIndexQ.enq(CRq (n));
            // reset link addr
            LineAddr repLineAddr = getLineAddr({ram.info.tag, truncate(procRq.addr)}); // index & bank are from procRq
            if(linkAddr == Valid (repLineAddr)) begin
                linkAddr <= Invalid;
            end
        endaction
        endfunction
    
        // function to set cRq to Depend, and make no further change to cache
        function Action cRqSetDepNoCacheChange;
        action
            cRqMshr.pipelineResp.setStateSlot(n, Depend, defaultValue);
            pipeline.deqWrite(Invalid, pipeOut.ram);
        endaction
        endfunction

        //(* split *)
        if(ram.info.owner matches tagged Valid .cOwner) (* nosplit *) begin
            //(* split *)
            if(cOwner != n) (* nosplit *) begin
                // owner is another cRq, so must just go through tag match
                // tag match must be hit (because replacement algo won't give a way with owner)
                doAssert(ram.info.cs > I && ram.info.tag == getTag(procRq.addr), 
                    ("cRq should hit in tag match")
                );
                // should be added to a cRq in dependency chain & deq from pipeline
                doAssert(isValid(cRqEOC), ("cRq hit on another cRq, cRqEOC must be true"));
                cRqMshr.pipelineResp.setSucc(fromMaybe(?, cRqEOC), Valid (n));
                cRqSetDepNoCacheChange;
                $display("%t L1 %m pipelineResp: cRq: own by other cRq ", $time,
                    fshow(cOwner), ", depend on cRq ", fshow(cRqEOC)
                );
            end
            else (* nosplit *) begin
                // owner is myself, so must be swapped in
                // tag should match, since always swapped in by cRq, cs > I
                doAssert(ram.info.tag == getTag(procRq.addr) && ram.info.cs > I,
                    "cRq swapped in by previous cRq, tag must match & cs > I"
                );
                // Hit or Miss (but no replacement)
                //(* split *)
                if(ram.info.cs >= procRq.toState) (* nosplit *) begin
                    $display("%t L1 %m pipelineResp: cRq: own by itself, hit", $time);
                    cRqHit(n, procRq);
                end
                else (* nosplit *) begin
                    $display("%t L1 %m pipelineResp: cRq: own by itself, miss no replace", $time);
                    cRqMissNoReplacement;
                end
            end
        end
        else (* nosplit *) begin
            // cache has no owner, cRq must just go through tag match
            // check for cRqEOC to append to dependency chain
            //(* split *)
            if(cRqEOC matches tagged Valid .k) (* nosplit *) begin
                $display("%t L1 %m pipelineResp: cRq: no owner, depend on cRq ", $time, fshow(k));
                cRqMshr.pipelineResp.setSucc(k, Valid (n));
                cRqSetDepNoCacheChange;
            end
            else (* nosplit *) begin
                //(* split *)
                if(ram.info.cs == I || ram.info.tag == getTag(procRq.addr)) (* nosplit *) begin
                    // No Replacement necessary
                    //(* split *)
                    if(ram.info.cs >= procRq.toState) (* nosplit *) begin
                        $display("%t L1 %m pipelineResp: cRq: no owner, hit", $time);
                        cRqHit(n, procRq);
                    end
                    else (* nosplit *) begin
                        $display("%t L1 %m pipelineResp: cRq: no owner, miss no replace", $time);
                        cRqMissNoReplacement;
                    end
                end
                else (* nosplit *) begin
                    $display("%t L1 %m pipelineResp: cRq: no owner, replace", $time);
                    cRqReplacement;
                end
            end
        end
    endrule

    rule pipelineResp_pRs(!isValid(processAmo) &&& pipeOut.cmd == L1PRs);
        $display("%t L1 %m pipelineResp: ", $time, fshow(pipeOut));
        $display("%t L1 %m pipelineResp: pRs: ", $time);

        if(ram.info.owner matches tagged Valid .cOwner) begin
            procRqT procRq = pipeOutCRq;
            doAssert(ram.info.cs >= procRq.toState && ram.info.tag == getTag(procRq.addr),
                ("pRs must be a hit")
            );
            cRqHit(cOwner, procRq);
`ifdef PERF_COUNT
            // performance counter: miss cRq
            incrMissCnt(procRq.op, cOwner);
`endif
        end
        else begin
            doAssert(False, ("pRs owner must match some cRq"));
        end
    endrule

    rule pipelineResp_pRq(!isValid(processAmo) &&& pipeOut.cmd matches tagged L1PRq .n);
        pRqFromPT pRq = pRqMshr.pipelineResp.getRq(n);
        $display("%t L1 %m pipelineResp: pRq: ", $time, fshow(n), " ; ", fshow(pRq));

        // pRq is never in dependency chain, so it is never swapped in
        // pRq must go through tag match, which either returns a tag matched way or asserts pRqMiss
        // and pRq is always directly handled: either dropped or Done

        if(pipeOut.pRqMiss || ram.info.cs <= pRq.toState || ram.info.tag != getTag(pRq.addr)) begin
            $display("%t L1 %m pipelineResp: pRq: drop", $time);
            // pRq can be directly dropped
            // must go through tag match, no successor
            pRqMshr.pipelineResp.releaseEntry(n);
            pipeline.deqWrite(Invalid, pipeOut.ram);
            // sanity check (ram.info.tag != getTag(pRq.addr) is useless)
            if(!pipeOut.pRqMiss) begin
                doAssert(ram.info.cs == S && pRq.toState == S && ram.info.tag == getTag(pRq.addr),
                    ("pRqMiss deasserted, must be down to S")
                );
            end
        end
        else if(ram.info.owner matches tagged Valid .cOwner) begin
            // XXX keep this getRq method call, this is involved in checking unsafe cRq MSHR
            procRqT cRq = pipeOutCRq;
            // must be the case the pRq overtakes cRq
            L1CRqState cState = cRqMshr.pipelineResp.getState(cOwner);
            cRqSlotT cSlot = cRqMshr.pipelineResp.getSlot(cOwner);
            $display("%t L1 %m pipelineResp: pRq: overtake cRq: ", $time,
                fshow(cOwner), " ; ",
                fshow(cRq), " ; ",
                fshow(cState), " ; ",
                fshow(cSlot)
            );
            doAssert(ram.info.cs == S && cRq.toState == M && pRq.toState == I && cState == WaitSt && cSlot.waitP,
                ("pRq overtakes CRq")
            );
            // process pRq
            pRqMshr.pipelineResp.setDone_setData(n, Invalid); // S->I, no data needed
            pipeline.deqWrite(Invalid, RamData {
                info: CacheInfo {
                    tag: ram.info.tag, // keep tag the same (for sake of cRq)
                    cs: I, // downgraded to I
                    dir: ?,
                    owner: ram.info.owner // keep owner to cRq
                },
                line: ram.line
            });
            rsToPIndexQ.enq(PRq (n));
            // update cRq bookkeeping
            // XXX keep this update, used in unsafe MSHR checking
            cRqMshr.pipelineResp.setStateSlot(cOwner, WaitSt, L1CRqSlot {
                way: pipeOut.way,
                cs: I, // update cs (actually useless...)
                repTag: ?,
                waitP: True
            });
        end
        else begin
            $display("%t L1 %m pipelineResp: pRq: valid process", $time);
            // line must NOT be owned
            doAssert(ram.info.owner == Invalid,
                ("pRq cannot hit on line owned by pRq (even itself)")
            );
            // should process pRq
            doAssert(ram.info.cs > pRq.toState && ram.info.tag == getTag(pRq.addr),
                ("pRq should be processed")
            );
            pRqMshr.pipelineResp.setDone_setData(n, ram.info.cs == M ? Valid (ram.line) : Invalid);
            pipeline.deqWrite(Invalid, RamData {
                info: CacheInfo {
                    tag: ram.info.tag,
                    cs: pRq.toState,
                    dir: ?,
                    owner: Invalid // no successor
                },
                line: ram.line
            });
            rsToPIndexQ.enq(PRq (n));
        end

        // since pRq is always processed in one shot, we reset link addr here together
        if(linkAddr == Valid (getLineAddr(pRq.addr)) && pRq.toState == I) begin
            linkAddr <= Invalid;
        end
    endrule

`ifdef SECURITY
    rule pipelineResp_flush(
        !isValid(processAmo) &&&
        !flushDone &&& !flushRespDone &&&
        pipeOut.cmd matches tagged L1Flush .flush
    );
        pRqIdxT n = flush.mshrIdx;
        $display("%t L1 %m pipelineResp: flush: ", $time, fshow(flush));

        // During flush, cRq MSHR is empty, so cache line cannot have owner
        doAssert(ram.info.owner == Invalid, "flushing line cannot have owner");

        // flush always goes through cache pipeline, and is directly handled
        // here: either dropped or Done
        if(ram.info.cs == I) begin
            $display("%t L1 %m pipelineResp: flush: drop", $time);
            // flush can be directly dropped
            pRqMshr.pipelineResp.releaseEntry(n);
        end
        else begin
            $display("%t L1 %m pipelineResp: flush: valid process", $time);
            pRqMshr.pipelineResp.setDone_setData(n, ram.info.cs == M ? Valid (ram.line) : Invalid);
            rsToPIndexQ.enq(PRq (n));
            // record the flushed addr in MSHR so that sendRsToP rule knows
            // which addr is invalidated
            Bit#(LgLineSzBytes) offset = 0;
            Addr addr = {ram.info.tag, flush.index, bankId, offset};
            pRqMshr.pipelineResp.setFlushAddr(n, addr);
        end

        // always clear the cache line
        pipeline.deqWrite(Invalid, RamData {
            info: CacheInfo {
                tag: ?,
                cs: I, // downgraded to I
                dir: ?,
                owner: Invalid // no successor
            },
            line: ?
        });

        // always reset link addr
        linkAddr <= Invalid;

        // check if we have finished all flush
        if (flush.index == maxBound &&
            pipeOut.way == fromInteger(valueof(wayNum) - 1)) begin
            flushRespDone <= True;
        end
    endrule

    rule completeFlush(!flushDone && flushReqStart && flushReqDone && flushRespDone);
        flushDone <= True;
        flushReqStart <= False;
        flushReqDone <= False;
        flushRespDone <= False;
    endrule
`endif

    // merge rq to parent index into indexQ
    rule rqIndexFromPipelineResp;
        let n <- toGet(rqToPIndexQ_pipelineResp).get;
        rqToPIndexQ.enq(n);
    endrule

    (* descending_urgency = "rqIndexFromPipelineResp, rqIndexFromSendRsToP" *)
    rule rqIndexFromSendRsToP;
        let n <- toGet(rqToPIndexQ_sendRsToP).get;
        rqToPIndexQ.enq(n);
    endrule

    interface ChildCacheToParent to_parent;
        interface rsToP = toFifoDeq(rsToPQ);
        interface rqToP = toFifoDeq(rqToPQ);
        interface fromP = toFifoEnq(fromPQ);
    endinterface

    interface L1ProcReq procReq;
        method Action req(procRqT r);
            rqFromCQ.enq(r);
        endmethod
    endinterface

    method Action resetLinkAddr;
        linkAddrRst <= Invalid;
    endmethod

    interface Get cRqStuck;
        method ActionValue#(L1CRqStuck) get;
            let s <- cRqMshr.stuck.get;
            return L1CRqStuck {
                addr: s.req.addr,
                op: s.req.op,
                state: s.state,
                slotCs: s.slotCs,
                waitP: s.waitP
            };
        endmethod
    endinterface
                
    interface pRqStuck = pRqMshr.stuck;

`ifdef SECURITY
    method Action flush if(flushDone);
        flushDone <= False;
    endmethod
    method flush_done = flushDone._read;
`else
    method flush = noAction;
    method flush_done = True;
`endif

    method Action setPerfStatus(Bool stats);
`ifdef PERF_COUNT
        doStats <= stats;
`else
        noAction;
`endif
    endmethod 

    method Data getPerfData(L1DPerfType t);
        return (case(t)
`ifdef PERF_COUNT
            L1DLdCnt: ldCnt;
            L1DStCnt: stCnt;
            L1DAmoCnt: amoCnt;
            L1DLdMissCnt: ldMissCnt;
            L1DStMissCnt: stMissCnt;
            L1DAmoMissCnt: amoMissCnt;
            L1DLdMissLat: ldMissLat;
            L1DStMissLat: stMissLat;
            L1DAmoMissLat: amoMissLat;
`endif
            default: 0;
        endcase);
    endmethod
endmodule


// Scheduling note

// cRqTransfer: write new cRq MSHR entry, cRqMshr.getEmptyEntry

// pRqTransfer: write new pRq MSHR entry, pRqMshr.getEmptyEntry

// pRsTransfer: -

// sendRsToP_cRq: access cRq MSHR entry that is sending replacement
// -- read req/state/slot/data
// -- write state/slot/data

// sendRsToP_pRq: read pRq MSHR entry that is responding, pRqMshr.releaseEntry

// sendRqToP: read cRq MSHR req/slot that is requesting parent

// pipelineResp_cRq:
// -- cRqMshr.releaseEntry
// -- read cRq MSHR req/state/slot/data currently processed
// -- write cRq MSHR state/slot/data currently processed
// -- write succ of some existing cRq MSHR entry (in WaitNewTag or WaitSt)
// -- read all state/req/succ in cRq MSHR entry (searchEOC)
//    -- not affected by write in cRqTransfer
//    -- not affected by write in sendRsToP_cRq (state change is just WaitNewTag->WaitSt)

// pipelineResp_pRs:
// -- cRqMshr.releaseEntry
// -- read cRqMSHR req/succ

// pipelineResp_pRq:
// -- r/w pRq MSHR entry, pRqMshr.releaseEntry
// -- read existing cRq state/req/slot, write its slot/state
//    -- cannot be the cRq in sendRsToP_cRq

// ---- conflict analysis ----

// XXXTransfer is conflict with each other
// but they are access new MSHR entry, so never conflict with other rules

// sendxxxToP is conflict free with each other

// sendRsToP_cRq conflict free with pipelineResp_xxx in terms of MSHR
// -- writes in sendRsToP_cRq does not affect r&w in pipelineResp_xxx
// -- writes in pipelineResp_xxx never affects r/w in sendRsToP_cRq
// BUT they may both enq to rqToPIndexQ

// sendRsToP_pRq conflict free with pipelineResp_xxx

// sendRqToP conflict free with pipelineResp_xxx

// ---- conclusion ----

// we have 4 ports from cRq MSHR
// 1. cRqTransfer
// 2. sendRsToP_cRq
// 3. sendRqToP
// 4. pipelineResp

// we have 3 ports from pRq MSHR
// 1. pRqTransfer
// 2. sendRsToP_pRq
// 3. pipelineResp


// unsafe version: all reads read the original reg value
// and all writes are cononicalized

// safe version: use EHR ports
// sendRsToP_cRq/sendRsToP_pRq: port 0
// pipelineResp: port 1
// cRqTransfer/sendRqToP/pRqTransfer: port 2

// We put cRqTransfer at last because it is ordered after the issue method of
// LSQ outside cache, while pipelineResp and sendRsToP will call other methods
// of LSQ. Making cRqTransfer after pipelineResp/sendRsToP will not add any
// extra ordering constraint to methods of LSQ.


// group banks into cache
module mkL1Cache#(
    module#(L1CRqMshr#(cRqNum, wayT, tagT, procRqT)) mkL1CRqMshrLocal,
    module#(L1PRqMshr#(pRqNum)) mkL1PRqMshrLocal,
    module#(L1Pipe#(lgBankNum, wayNum, indexT, tagT, cRqIdxT, pRqIdxT)) mkL1Pipeline,
    L1ProcResp#(procRqIdT) procResp
)(
    L1Bank#(lgBankNum, wayNum, indexSz, tagSz, cRqNum, pRqNum, procRqIdT)
) provisos (
    NumAlias#(bankNum, TExp#(lgBankNum)),
    Alias#(bankIdT, Bit#(lgBankNum)),
    Alias#(l1BankT, L1Bank#(lgBankNum, wayNum, indexSz, tagSz, cRqNum, pRqNum, procRqIdT)),
    Alias#(wayT, Bit#(TLog#(wayNum))),
    Alias#(indexT, Bit#(indexSz)),
    Alias#(tagT, Bit#(tagSz)),
    Alias#(cRqIdxT, Bit#(TLog#(cRqNum))),
    Alias#(pRqIdxT, Bit#(TLog#(pRqNum))),
    Alias#(cacheOwnerT, Maybe#(cRqIdxT)),
    Alias#(procRqT, ProcRq#(procRqIdT)),
    Alias#(cRqToPT, CRqMsg#(wayT, void)),
    Alias#(cRsToPT, CRsMsg#(void)),
    Alias#(pRqRsFromPT, PRqRsMsg#(wayT, void)),
    Alias#(l1CmdT, L1Cmd#(indexT, cRqIdxT, pRqIdxT)),
    Alias#(pipeOutT, PipeOut#(wayT, tagT, Msi, void, cacheOwnerT, Line, l1CmdT)),
    // requirements
    Bits#(procRqIdT, _procRqIdT),
    FShow#(procRqIdT),
    FShow#(pipeOutT),
    Add#(tagSz, a__, AddrSz),
    // make sure: cRqNum <= wayNum
    Add#(cRqNum, b__, wayNum),
    Add#(lgBankNum, c__, AddrSz),
    Add#(1, d__, bankNum),
    Add#(TAdd#(tagSz, indexSz), TAdd#(lgBankNum, LgLineSzBytes), AddrSz)
);
    // bank id of each cache bank is implicit, we always send fixed subset of address to a bank
    // the pipelineResp_cRq,pRs will conflict with each other
    Vector#(bankNum, l1BankT) banks;
    for (Integer i = 0; i < valueof(bankNum); i = i+1) begin
        banks[i] <- mkL1Bank(fromInteger(i), mkL1CRqMshrLocal, mkL1PRqMshrLocal, mkL1Pipeline, procResp);
    end

    function bankIdT getBankId(Addr a);
        return truncate(a >> valueof(LgLineSzBytes));
    endfunction

    ChildCacheToParent#(wayT, void) toParentIfc; // ifc to parent cache

    if(valueof(bankNum) == 1) begin
        toParentIfc = banks[0].to_parent;
    end
    else begin
        // multiple banks need cross bar
        Fifo#(2, cRqToPT) cRqToPQ <- mkCFFifo;
        Fifo#(2, cRsToPT) cRsToPQ <- mkCFFifo;
        Fifo#(2, pRqRsFromPT) pRqRsFromPQ <- mkCFFifo;

        function XBarDstInfo#(Bit#(0), cRqToPT) getCRqDstInfo(bankIdT bid, cRqToPT cRq);
            return XBarDstInfo {idx: 0, data: cRq};
        endfunction
        function Get#(cRqToPT) cRqGet(l1BankT ifc) = toGet(ifc.to_parent.rqToP);
        mkXBar(getCRqDstInfo, map(cRqGet, banks), vec(toPut(cRqToPQ)));

        function XBarDstInfo#(Bit#(0), cRsToPT) getCRsDstInfo(bankdIdT bid, cRsToPT cRs);
            return XBarDstInfo {idx: 0, data: cRs};
        endfunction
        function Get#(cRsToPT) cRsGet(l1BankT ifc) = toGet(ifc.to_parent.rsToP);
        mkXBar(getCRsDstInfo, map(cRsGet, banks), vec(toPut(cRsToPQ)));

        for(Integer i = 0; i < valueof(bankNum); i = i+1) begin
            rule sendPRq(pRqRsFromPQ.first matches tagged PRq .rq &&& getBankId(rq.addr) == fromInteger(i));
                let r <- toGet(pRqRsFromPQ).get;
                banks[i].to_parent.fromP.enq(r);
            endrule
            rule sendPRs(pRqRsFromPQ.first matches tagged PRs .rs &&& getBankId(rs.addr) == fromInteger(i));
                let r <- toGet(pRqRsFromPQ).get;
                banks[i].to_parent.fromP.enq(r);
            endrule
        end

        toParentIfc = (interface ChildCacheToParent;
            interface rqToP = toFifoDeq(cRqToPQ);
            interface rsToP = toFifoDeq(cRsToPQ);
            interface fromP = toFifoEnq(pRqRsFromPQ);
        endinterface);
    end

`ifdef CHECK_DEADLOCK
    FIFO#(L1CRqStuck) cRqStuckQ <- mkFIFO1;
    FIFO#(L1PRqStuck) pRqStuckQ <- mkFIFO1;

    for(Integer i = 0; i < valueof(bankNum); i = i+1) begin
        rule sendCRqStuck;
            let s <- banks[i].cRqStuck.get;
            cRqStuckQ.enq(s);
        endrule
        rule sendPRqStuck;
            let s <- banks[i].pRqStuck.get;
            pRqStuckQ.enq(s);
        endrule
    end

    interface cRqStuck = toGet(cRqStuckQ);
    interface pRqStuck = toGet(pRqStuckQ);
`else
    interface cRqStuck = nullGet;
    interface pRqStuck = nullGet;
`endif

    interface ChildCacheToParent to_parent = toParentIfc;

    interface L1ProcReq procReq;
        method Action req(procRqT r);
            banks[getBankId(r.addr)].procReq.req(r);
        endmethod
    endinterface

    method Action resetLinkAddr;
        for(Integer i = 0; i < valueof(bankNum); i = i+1) begin
            banks[i].resetLinkAddr;
        end
    endmethod

    method Action flush;
        for(Integer i = 0; i < valueof(bankNum); i = i+1) begin
            banks[i].flush;
        end
    endmethod

    method Bool flush_done;
        Vector#(bankNum, Bool) b;
        for(Integer i = 0; i < valueof(bankNum); i = i+1) begin
            b[i] = banks[i].flush_done;
        end
        return fold(\&& , b);
    endmethod

    method Action setPerfStatus(Bool stats);
        for(Integer i = 0; i < valueof(bankNum); i = i+1) begin
            banks[i].setPerfStatus(stats);
        end
    endmethod

    method Data getPerfData(L1DPerfType t);
        Vector#(bankNum, Data) d = ?;
        for(Integer i = 0; i < valueof(bankNum); i = i+1) begin
            d[i] = banks[i].getPerfData(t);
        end
        return fold(\+ , d);
    endmethod
endmodule
