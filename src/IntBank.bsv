
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

import Vector::*;
import FIFO::*;
import GetPut::*;
import Types::*;
import CCTypes::*;
import CRqMshr::*;
import PRqMshr::*;
import CCPipe::*;
import IntPipe ::*;
import FShow::*;
import DefaultValue::*;
import Fifo::*;
import CacheUtils::*;

// although pRq never appears in dependency chain
// we still need pRq MSHR to limit the number of pRq
// and thus limit the size of rsToPIndexQ

// XXX we need to maintain the invariant that 
// at most 1 pRq sent to a child for an addr
// and we have to wait for resp before sending anthoer one

// In future, it would be hard to parallelize sendRqToC and pipelineResp
// because sendRqToC must see updates to dirPend made in pipelineResp
// or we simply stall sendRqToC

interface IntBank#(
    numeric type lgBankNum,
    numeric type childNum,
    numeric type wayNum,
    numeric type indexSz,
    numeric type tagSz,
    numeric type cRqNum,
    numeric type pRqNum,
    type cRqIdT
);
    interface FifoEnq#(CRqMsg#(cRqIdT, Bit#(TLog#(childNum)))) rqFromCPut;
    interface FifoEnq#(CRsMsg#(Bit#(TLog#(childNum)))) rsFromCPut;
    interface FifoDeq#(PRqRsMsg#(cRqIdT, Bit#(TLog#(childNum)))) toCGet;

    interface FifoDeq#(CRsMsg#(void)) rsToPGet;
    interface FifoDeq#(CRqMsg#(Bit#(TLog#(wayNum)), void)) rqToPGet;
    interface FifoEnq#(PRqRsMsg#(Bit#(TLog#(wayNum)), void)) fromPPut;
endinterface

module mkIntBank#(
    module#(CRqMshr#(childNum, cRqNum, pRqIndexT, wayT, tagT, cRqFromCT)) mkIntCRqMshr,
    module#(PRqMshr#(childNum, pRqNum, cRqIndexT, wayT)) mkIntPRqMshr,
    module#(IntPipe#(lgBankNum, childNum, wayNum, indexT, tagT, cRqIndexT, pRqIndexT)) mkIntPipeline
)(
    IntBank#(lgBankNum, childNum, wayNum, indexSz, tagSz, cRqNum, pRqNum, cRqIdT)
) provisos(
    Alias#(childT, Bit#(TLog#(childNum))),
    Alias#(wayT, Bit#(TLog#(wayNum))),
    Alias#(indexT, Bit#(indexSz)),
    Alias#(tagT, Bit#(tagSz)),
    Alias#(dirT, Vector#(childNum, Msi)),
    Alias#(cRqIndexT, Bit#(TLog#(cRqNum))),
    Alias#(pRqIndexT, Bit#(TLog#(pRqNum))),
    Alias#(mshrIdxT, MshrIndex#(cRqIndexT, pRqIndexT)),
    Alias#(cacheOwnerT, CacheOwner#(cRqIndexT, pRqIndexT)),
    Alias#(cacheInfoT, CacheInfo#(tagT, Msi, dirT, cacheOwnerT)),
    Alias#(ramDataT, RamData#(tagT, Msi, dirT, cacheOwnerT, Line)),
    Alias#(cRqFromCT, CRqMsg#(cRqIdT, childT)),
    Alias#(cRsFromCT, CRsMsg#(childT)),
    Alias#(pRqRsToCT, PRqRsMsg#(cRqIdT, childT)),
    Alias#(cRqToPT, CRqMsg#(wayT, void)),
    Alias#(cRsToPT, CRsMsg#(void)),
    Alias#(pRqFromPT, PRqMsg#(void)),
    Alias#(pRsFromPT, PRsMsg#(wayT, void)),
    Alias#(pRqRsFromPT, PRqRsMsg#(wayT, void)),
    Alias#(cRqSlotT, CRqSlot#(childNum, wayT, tagT)), // cRq MSHR slot
    Alias#(pRqSlotT, PRqSlot#(childNum, wayT)), // pRq MSHR slot
    Alias#(intCmdT, IntCmd#(childT, cRqIndexT, pRqIndexT)),
    Alias#(pipeOutT, PipeOut#(wayT, tagT, Msi, dirT, cacheOwnerT, Line, intCmdT)),
    // requirements
    Alias#(cRqIdT, Bit#(_cRqIdSz)),
    Add#(tagSz, a__, AddrSz),
    // make sure: cRqNum <= wayNum
    Add#(cRqNum, b__, wayNum)
);

    CRqMshr#(childNum, cRqNum, pRqIndexT, wayT, tagT, cRqFromCT) cRqMshr <- mkIntCRqMshr;

    PRqMshr#(childNum, pRqNum, cRqIndexT, wayT) pRqMshr <- mkIntPRqMshr;

    IntPipe#(lgBankNum, childNum, wayNum, indexT, tagT, cRqIndexT, pRqIndexT) pipeline <- mkIntPipeline;

    Fifo#(2, cRqFromCT) rqFromCQ <- mkCFFifo;

    Fifo#(2, cRsFromCT) rsFromCQ <- mkCFFifo;

    Fifo#(2, pRqRsToCT) toCQ <- mkCFFifo;

    Fifo#(2, cRsToPT) rsToPQ <- mkCFFifo;

    Fifo#(2, cRqToPT) rqToPQ <- mkCFFifo;

    Fifo#(2, pRqRsFromPT) fromPQ <- mkCFFifo;

    FIFO#(mshrIdxT) rsToPIndexQ <- mkSizedFIFO(valueOf(TAdd#(cRqNum, pRqNum)));

    FIFO#(cRqIndexT) rqToPIndexQ <- mkSizedFIFO(valueOf(cRqNum));

    Fifo#(cRqNum, cRqIndexT) rsToCIndexQ <- mkCFFifo;

    function tagT getTag(Addr a) = truncateLSB(a);

    rule cRqTransfer;
        rqFromCQ.deq;
        cRqFromCT cRq = rqFromCQ.first;
        Maybe#(cRqIndexT) emptySlot = cRqMshr.getEmptySlot;
        check(isValid(emptySlot));
        cRqIndexT n = fromMaybe(?, emptySlot);
        cRqMshr.setRq(n, cRq);
        cRqMshr.setStateSlot(n, Init, defaultValue);
        cRqMshr.setSucc(n, Invalid);
        pipeline.send(CRq (IntPipeRqIn {
            addr: cRq.addr, 
            mshrIdx: n
        }));
        $display("%t Int %m cRqTransfer: ", $time, 
            fshow(n), " ; ",
            fshow(cRq)
        );
    endrule
    
    rule cRsTransfer;
        rsFromCQ.deq;
        cRsFromCT cRs = rsFromCQ.first;
        pipeline.send(CRs (cRs));
        $display("%t Int %m cRsTransfer: ", $time, fshow(cRs));
    endrule
    
    rule pRqTransfer(fromPQ.first matches tagged PRq .pRq);
        fromPQ.deq;
        Maybe#(pRqIndexT) emptySlot = pRqMshr.getEmptySlot;
        check(isValid(emptySlot));
        pRqIndexT n = fromMaybe(?, emptySlot);
        pRqMshr.setRq(n, pRq);
        pRqMshr.setStateSlot(n, Init, defaultValue);
        pRqMshr.setSucc(n, Invalid);
        pipeline.send(PRq (IntPipeRqIn {
            addr: pRq.addr,
            mshrIdx: n
        }));
        $display("%t Int %m pRqTransfer: ", $time, 
            fshow(n), " ; ", 
            fshow(pRq)
        );
    endrule

    rule pRsTransfer(fromPQ.first matches tagged PRs .pRs);
        fromPQ.deq;
        pipeline.send(PRs (IntPipePRsIn {
            addr: pRs.addr,
            toState: pRs.toState,
            data: pRs.data,
            way: pRs.id
        }));
        $display("%t Int %m pRsTransfer: ", $time, fshow(pRs));
    endrule
    
    rule sendRsToP;
        rsToPIndexQ.deq;
        if(rsToPIndexQ.first matches tagged CRq .n) begin
            cRqFromCT cRq <- cRqMshr.getRq(n);
            cRqSlotT cSlot <- cRqMshr.getSlot(n);
            CRqState cState <- cRqMshr.getState(n);
            doAssert(cState == WaitNewTag, 
                "send replacement resp to parent, state should be WaitNewTag" 
            );
            // send resp to parent
            cRsToPT rsToP = CRsMsg {
                addr: {cSlot.repTag, truncate(cRq.addr)}, // get bank id & index from cRq
                toState: I,
                data: cSlot.data,
                child: ?
            };
            rsToPQ.enq(rsToP);
            // req parent for upgrade & change state
            rqToPIndexQ.enq(n);
            cRqMshr.setStateSlot(n, WaitSt, CRqSlot {
                way: cSlot.way,
                cs: I, // replacement, so I (get ready for rqToIndex.deq)
                repTag: ?, // no longer needed
                waitP: True, // we have req parent at the same time
                dirPend: replicate(Invalid), // children are all I, cannot be waiting
                data: Invalid // no data now
            });
            $display("%t Int %m sendRsToP: ", $time, 
                fshow(rsToPIndexQ.first)," ; ", 
                fshow(cRq), " ; ", 
                fshow(rsToP)
            );
        end
        else if(rsToPIndexQ.first matches tagged PRq .n) begin
            pRqFromPT pRq <- pRqMshr.getRq(n);
            pRqSlotT pSlot <- pRqMshr.getSlot(n);
            cRsToPT rsToP = CRsMsg {
                addr: pRq.addr,
                toState: pRq.toState,
                data: pSlot.data,
                child: ?
            };
            rsToPQ.enq(rsToP);
            pRqMshr.setStateSlot(n, Empty, ?);
            $display("%t Int %m sendRsToP: ", $time, 
                fshow(rsToPIndexQ.first), " ; ", 
                fshow(pRq), " ; ", 
                fshow(rsToP)
            );
        end
    endrule

    rule sendRqToP;
        rqToPIndexQ.deq;
        cRqIndexT n = rqToPIndexQ.first;
        cRqFromCT cRq <- cRqMshr.getRq(n);
        cRqSlotT cSlot <- cRqMshr.getSlot(n);
        cRqToPT rqToP = CRqMsg {
            addr: cRq.addr,
            fromState: cSlot.cs,
            toState: cRq.toState,
            id: cSlot.way,
            child: ?
        };
        rqToPQ.enq(rqToP);
        $display("%t Int %m sendRqToP: ", $time, 
            fshow(n), " ; ", 
            fshow(cRq), " ; ", 
            fshow(cSlot), " ; ", 
            fshow(rqToP)
        );
    endrule

    rule sendRsToC(rsToCIndexQ.notEmpty);
        // send upgrade resp to child
        rsToCIndexQ.deq;
        cRqIndexT n = rsToCIndexQ.first;
        cRqFromCT cRq <- cRqMshr.getRq(n);
        cRqSlotT cSlot <- cRqMshr.getSlot(n);
        pRqRsToCT rsToC = PRs (PRsMsg {
            addr: cRq.addr,
            toState: cRq.toState,
            child: cRq.child,
            data: cSlot.data,
            id: cRq.id
        });
        toCQ.enq(rsToC);
        // release MSHR entry
        cRqMshr.setStateSlot(n, Empty, ?);
        $display("%t Int %m sendRsToC: ", $time, 
            fshow(n), " ; ", 
            fshow(cRq), " ; ", 
            fshow(rsToC)
        );
    endrule

    // round robin select cRq to downgrade child
    // but downgrade must wait for all upgrade resp
    Reg#(cRqIndexT) whichCRq <- mkReg(0);
    rule sendRqToC_cRq(!rsToCIndexQ.notEmpty);
        Maybe#(cRqIndexT) cRqNeedDown = cRqMshr.searchNeedRqChild(Valid (whichCRq));
        // XXX must add this into guard
        // otherwise this rule will block pipelineResp rule from firing forever
        check(isValid(cRqNeedDown));

        cRqIndexT n = fromMaybe(?, cRqNeedDown);
        cRqFromCT cRq <- cRqMshr.getRq(n);
        cRqSlotT cSlot <- cRqMshr.getSlot(n);
        CRqState cState <- cRqMshr.getState(n);
        doAssert(cState == WaitSt || cState == WaitOldTag, 
            "only WaitSt and WaitOldTag needs req child"
        );
        // find a child to downgrade
        function Bool needSend(DirPend dp);
            return dp matches tagged ToSend .s ? True : False;
        endfunction
        Maybe#(childT) childToDown = searchIndex(needSend, cSlot.dirPend);
        doAssert(isValid(childToDown), ("should have a child to downgrade"));
        childT child = fromMaybe(?, childToDown);
        if(cSlot.dirPend[child] matches tagged ToSend .st) begin
            // send downgrade req: addr depends on state
            // either be cRq addr or replacing addr
            Addr rqAddr = cState == WaitSt ? cRq.addr : {cSlot.repTag, truncate(cRq.addr)};
            pRqRsToCT req = PRq (PRqMsg {
                addr: rqAddr,
                toState: st,
                child: child
            });
            toCQ.enq(req);
            // change dirPend
            cRqSlotT newCSlot = cSlot;
            newCSlot.dirPend[child] = Waiting (st);
            cRqMshr.setStateSlot(n, cState, newCSlot);
            doAssert(!isValid(cSlot.data), "data must be invalid when req child");
            $display("%t Int %m sendRqToC_cRq: ", $time,
                fshow(n), " ; ",
                fshow(cRq), " ; ",
                fshow(cSlot), " ; ",
                fshow(cState), " ; ",
                fshow(req)
            );
        end
        else begin
            doAssert(False, ("dirPend should be ToSend"));
        end
        // change round-robin
        whichCRq <= whichCRq == fromInteger(valueOf(cRqNum) - 1) ? 0 : whichCRq + 1;
    endrule

    // round robin to select pRq to downgrade child
    Reg#(pRqIndexT) whichPRq <- mkReg(0);
    rule sendRqToC_pRq(!rsToCIndexQ.notEmpty);
        Maybe#(pRqIndexT) pRqNeedDown = pRqMshr.searchProcessing(Valid (whichPRq));
        // XXX must add this into guard
        // otherwise this rule will block pipelineResp rule from firing forever
        check(isValid(pRqNeedDown));

        pRqIndexT n = fromMaybe(?, pRqNeedDown);
        pRqFromPT pRq <- pRqMshr.getRq(n);
        pRqSlotT pSlot <- pRqMshr.getSlot(n);
        PRqState pState <- pRqMshr.getState(n);
        doAssert(pState == Processing, 
            "only Processing needs req child"
        );
        // find a child to downgrade
        function Bool needSend(DirPend dp);
            return dp matches tagged ToSend .s ? True : False;
        endfunction
        Maybe#(childT) childToDown = searchIndex(needSend, pSlot.dirPend);
        doAssert(isValid(childToDown), ("should have a child to downgrade"));
        childT child = fromMaybe(?, childToDown);
        if(pSlot.dirPend[child] matches tagged ToSend .st) begin
            pRqRsToCT req = PRq (PRqMsg {
                addr: pRq.addr, // addr from pRq
                toState: st,
                child: child
            });
            toCQ.enq(req);
            // change dirPend
            pRqSlotT newPSlot = pSlot;
            newPSlot.dirPend[child] = Waiting (st);
            pRqMshr.setStateSlot(n, pState, newPSlot);
            doAssert(!isValid(pSlot.data), "data must be invalid when req child");
            $display("%t Int %m sendRqToC_pRq: ", $time,
                fshow(n), " ; ",
                fshow(pRq), " ; ",
                fshow(pSlot), " ; ",
                fshow(pState), " ; ",
                fshow(req)
            );
        end
        else begin
            doAssert(False, ("dirPend should be ToSend"));
        end
        // change round-robin
        whichPRq <= whichPRq == fromInteger(valueOf(pRqNum) - 1) ? 0 : whichPRq + 1;
    endrule


    rule pipelineResp;
        pipeOutT pipeOut = pipeline.first;
        $display("%t Int %m pipelineResp: ", $time, fshow(pipeOut));
        ramDataT ram = pipeOut.ram;

        // function check whether dir is compatible to cRq
        function Bool dirAllCompat(cRqFromCT cRq);
            Msi compatSt = toCompat(cRq.toState);
            function Bool isCompat(childT i);
                if(i == cRq.child) begin
                    return ram.info.dir[i] <= cRq.fromState;
                end
                else begin
                    return ram.info.dir[i] <= compatSt;
                end
            endfunction
            Vector#(childNum, childT) idxVec = genWith(fromInteger);
            return all(isCompat, idxVec);
        endfunction

        // function to process cRq hit (MSHR slot may have garbage)
        function Action cRqHit(cRqIndexT n, cRqFromCT cRq);
        action
            $display("%t Int %m pipelineResp: cRq Hit func: ", $time, 
                fshow(n), " ; ", 
                fshow(cRq) 
            );
            doAssert(ram.info.tag == getTag(cRq.addr) && ram.info.cs >= cRq.toState && dirAllCompat(cRq),
                // even this function is called by pRs
                // tag should be written into cache before sending req to parent
                // dir should be in right states
                ("cRqHit conditions")
            );
            // update slot, data & send to indexQ
            // decide data validity using dir (which is more up to date than fromState)
            rsToCIndexQ.enq(n);
            cRqMshr.setStateSlot(n, Done, CRqSlot { // only data is useful
                way: ?,
                cs: ?,
                repTag: ?,
                dirPend: ?,
                waitP: ?,
                data: ram.info.dir[cRq.child] == I ? Valid (ram.line) : Invalid
            });
            // update child dir
            dirT newDir = ram.info.dir;
            newDir[cRq.child] = cRq.toState;
            // deq pipeline or swap in successor
            Maybe#(mshrIdxT) succ <- cRqMshr.getSucc(n);
            pipeline.deqWrite(succ, RamData {
                info: CacheInfo {
                    tag: getTag(cRq.addr), // should be the same as original tag
                    cs: ram.info.cs, // use cs in ram, because ram.info.cs > req.toState is possible
                    dir: newDir,
                    owner: (case(succ) matches // pass owner to successor
                        tagged Valid (tagged CRq .m): return CRq (CRqOwner {
                            mshrIdx: m,
                            replacing: False // swapped in cRq never replace
                        });
                        tagged Valid (tagged PRq .m): return PRq (PRqOwner {
                            mshrIdx: m,
                            hasSucc: False // this will be set correctly next cycle
                            // due to swapping, tag match stage is stalled
                        });
                        default: return Invalid;
                    endcase)
                },
                line: ram.line
            });
        endaction
        endfunction

        // function to directly evict a cache line by a cRq (replacement)
        function Action cRqEvict(cRqIndexT n, cRqFromCT cRq);
        action
            doAssert(ram.info.dir == replicate(I) && ram.info.cs > I,
                "only evict valid line which has no children"
            );
            // send to indexQ
            rsToPIndexQ.enq(CRq (n));
            // update MSHR
            cRqMshr.setStateSlot(n, WaitNewTag, CRqSlot {
                way: pipeOut.way, // use way from pipeline (cSlot.way may be garbage)
                cs: I,
                repTag: ram.info.tag, // used to assemble addr in resp
                waitP: False, // cannot req parent until this resp is truly sent to network
                dirPend: replicate(Invalid), // children are all I
                data: ram.info.cs == M ? Valid (ram.line) : Invalid // data in resp
            });
            // deq pipe & change RAM
            pipeline.deqWrite(Invalid, RamData {
                info: CacheInfo {
                    tag: getTag(cRq.addr), // set to new tag (old tag is replaced)
                    cs: I,
                    dir: replicate(I),
                    owner: CRq (CRqOwner {
                        mshrIdx: n, // owner is current cRq
                        replacing: False // replacement is done (tag is updated in RAM)
                    })
                },
                line: ? // data is no longer used
            });
        endaction
        endfunction

        // derive dirPend for children who have state > st
        // i.e. used in replacement and pRq
        function Vector#(childNum, DirPend) getDirPendGtSt(Msi st);
            function DirPend getDP(Integer i);
                return ram.info.dir[i] > st ? ToSend (st) : Invalid;
            endfunction
            return map(getDP, genVector);
        endfunction

        // check dir are all <= certain value
        // i.e. used in replacement and pRq
        function Bool dirAllLeSt(Msi st);
            function Bool isLe(Msi dir);
                return dir <= st;
            endfunction
            return all(isLe, ram.info.dir);
        endfunction

        // function to finish a pRq
        function Action pRqDone(pRqIndexT n, pRqFromPT pRq);
        action
            $display("%t Int %m pipelineResp: pRq Done func: ", $time, 
                fshow(n), " ; ", 
                fshow(pRq) 
            );
            doAssert(ram.info.tag == getTag(pRq.addr) && ram.info.cs > pRq.toState && dirAllLeSt(pRq.toState),
                "pRq resp conditions"
            );
            // send to indexQ
            rsToPIndexQ.enq(PRq (n));
            // set pRq to Done (only data field matters)
            pRqMshr.setStateSlot(n, Done, PRqSlot {
                way: ?,
                dirPend: ?,
                data: ram.info.cs == M ? Valid (ram.line) : Invalid
            });
            // deq pipe & swap in & change ram
            // (tag, dir, line should be kept)
            // (cs and owner may change)
            Maybe#(cRqIndexT) succ <- pRqMshr.getSucc(n);
            pipeline.deqWrite(succ matches tagged Valid .m ? Valid (CRq (m)) : Invalid,
                RamData {
                    info: CacheInfo {
                        tag: ram.info.tag,
                        cs: pRq.toState,
                        dir: ram.info.dir,
                        owner: (case(succ) matches 
                            tagged Valid .m: return CRq (CRqOwner {
                                mshrIdx: m,
                                replacing: False // swapped in cRq never replace
                            });
                            default: return Invalid;
                        endcase)
                    },
                    line: ram.line
                }
            );
        endaction
        endfunction

        if(pipeOut.cmd matches tagged IntCRq .n) begin // handle cRq
            cRqFromCT cRq <- cRqMshr.getRq(n);
            CRqState cState <- cRqMshr.getState(n);

            // find end of dependency chain
            Maybe#(cRqIndexT) cRqEOC <- cRqMshr.searchEndOfChain(cRq.addr);
            Maybe#(pRqIndexT) pRqEOC <- pRqMshr.searchEndOfChain(cRq.addr);
            $display("%t Int %m pipelineResp: cRq: ", $time,
                fshow(n), " ; ", 
                fshow(cRq), " ; ",
                fshow(cState), " ; ",
                fshow(cRqEOC), " ; ",
                fshow(pRqEOC)
            );
            // EOC at most one
            doAssert(!(isValid(cRqEOC) && isValid(pRqEOC)),
                "at most 1 EOC for an addr"
            );

            // function to check whether children needs downgrade (when miss no replace)
            function Vector#(childNum, DirPend) getDirPendNonCompat;
                function DirPend initPend(childT i);
                    if(i == cRq.child) begin
                        return ram.info.dir[i] <= cRq.fromState ? Invalid : Waiting (cRq.fromState);
                    end
                    else begin
                        Msi compatState = toCompat(cRq.toState);
                        return ram.info.dir[i] <= compatState ? Invalid : ToSend (compatState);
                    end
                endfunction
                Vector#(childNum, childT) idxVec = genWith(fromInteger);
                return map(initPend, idxVec);
            endfunction

            // function to process cRq miss without replacement (MSHR slot may have garbage)
            function Action cRqMissNoReplacement(Vector#(childNum, DirPend) dirPend);
            action
                cRqSlotT cSlot <- cRqMshr.getSlot(n);
                if(ram.info.cs < cRq.toState && !cSlot.waitP) begin
                    // waitP field is False (instead of garbage) if this is a new req
                    // must check waitP, since it could be waken up by the switch of cRq & pRq
                    rqToPIndexQ.enq(n);
                end
                // update mshr
                cRqMshr.setStateSlot(n, WaitSt, CRqSlot {
                    way: pipeOut.way, // use way from pipeline (cSlot may be garbage)
                    cs: ram.info.cs, // record cs for future rqToPIndexQ.deq
                    waitP: ram.info.cs < cRq.toState,
                    repTag: ?, // no replacement
                    dirPend: dirPend, // set dir pend
                    data: Invalid
                });
                // deq pipeline & set owner, tag
                pipeline.deqWrite(Invalid, RamData {
                    info: CacheInfo {
                        tag: getTag(cRq.addr), // tag may be garbage if cs == I
                        cs: ram.info.cs,
                        dir: ram.info.dir,
                        owner: CRq (CRqOwner {mshrIdx: n, replacing: False}) // owner is req itself
                    },
                    line: ram.line
                });
            endaction
            endfunction
    
            // function to start replacement for cRq
            // i.e. cannot directly evict, need to wait children to downgrade
            function Action cRqStartReplace(Vector#(childNum, DirPend) dirPend);
            action
                doAssert(dirPend != replicate(Invalid), "some child should need downgrade");
                pipeline.deqWrite(Invalid, RamData {
                    info: CacheInfo {
                        tag: ram.info.tag, // keep old tag here
                        cs: ram.info.cs,
                        dir: ram.info.dir,
                        owner: CRq (CRqOwner {
                            mshrIdx: n,
                            replacing: True // replacement is ongoing
                        })
                    },
                    line: ram.line // keep data the same
                });
                cRqMshr.setStateSlot(n, WaitOldTag, CRqSlot {
                    way: pipeOut.way,
                    cs: ram.info.cs,
                    repTag: ram.info.tag, // record tag for downgrading children
                    waitP: False, // cannot req parent yet
                    dirPend: dirPend, // set dir pend
                    data: Invalid
                });
            endaction
            endfunction
        
            // function to set cRq to Depend, and make no further change to cache
            // (XXX In some cases PRqOwner.hasSucc may be set, so cannot use this func)
            function Action cRqSetDepNoCacheChange;
            action
                cRqMshr.setStateSlot(n, Depend, defaultValue);
                pipeline.deqWrite(Invalid, pipeOut.ram);
            endaction
            endfunction

            if(ram.info.owner matches tagged CRq .cOwner) begin
                if(cOwner.mshrIdx != n) begin
                    $display("%t Int %m pipelineResp: cRq: own by other cRq, make depend: ", $time,
                        fshow(cOwner.mshrIdx)
                    );
                    // owner is another cRq, so must just go through tag match
                    // tag match must be hit 
                    // because replacement algo won't give a way owned by cRq
                    doAssert(cState == Init && ram.info.cs > I && ram.info.tag == getTag(cRq.addr), 
                        ("cRq should hit in tag match")
                    );
                    // should be added to a cRq in dependency chain
                    doAssert(isValid(cRqEOC) != isValid(pRqEOC),
                        "cRq hit on another cRq, there must be exactly one EOC"
                    );
                    if(cRqEOC matches tagged Valid .m) begin
                        cRqMshr.setSucc(m, Valid (CRq (n)));
                    end
                    if(pRqEOC matches tagged Valid .m) begin
                        pRqMshr.setSucc(m, Valid (n));
                    end
                    // deq pipeline & no change to cache 
                    // (even append to pRq, pRq is not cache owner, so no change to RAM)
                    cRqSetDepNoCacheChange;
                end
                else begin
                    // owner is myself, so must be swapped in
                    // tag should match, but cache state may be I
                    doAssert(cState == Depend && ram.info.tag == getTag(cRq.addr), 
                        "cRq swapped in, tag must match"
                    );
                    // check dir, get dir pend
                    Vector#(childNum, DirPend) dirPend = getDirPendNonCompat;
                    doAssert((dirPend == replicate(Invalid)) == dirAllCompat(cRq),
                        "dirPend same as dir compat"
                    );
                    // Hit or Miss (but no replacement)
                    if(ram.info.cs >= cRq.toState && dirAllCompat(cRq)) begin
                        // Hit
                        $display("%t Int %m pipelineResp: cRq: own by itself, hit", $time);
                        cRqHit(n, cRq);
                    end
                    else begin
                        // miss
                        doAssert(ram.info.cs < cRq.toState || dirPend != replicate(Invalid),
                            "must be waiting for some state change"
                        );
                        $display("%t Int %m pipelineResp: cRq: own by itself, miss no replace", $time,
                            fshow(dirPend)
                        );
                        cRqMissNoReplacement(dirPend);
                    end
                end
            end
            else if(ram.info.owner matches tagged PRq .pOwner) begin
                $display("%t Int %m pipelineResp: cRq: own by pRq: ", $time, fshow(pOwner));
                // must just go through tag match, have 2 possibilities
                // 1. tag match hit
                // 2. replacement i.e. this pRq has no successor
                // this line must be valid since it's owned by pRq
                doAssert(cState == Init && ram.info.cs > I, 
                    "cRq gets line owned by pRq which must own valid line"
                );
                // check 2 cases separately
                if(ram.info.tag == getTag(cRq.addr)) begin
                    $display("%t Int %m pipelineResp: cRq: own by pRq: tag match, make depend", $time);
                    // tag match hit case, add to dep chain
                    doAssert(isValid(cRqEOC) != isValid(pRqEOC),
                        "cRq hit on another pRq, there must be exactly one EOC"
                    );
                    if(cRqEOC matches tagged Valid .m) begin
                        cRqMshr.setSucc(m, Valid (CRq (n)));
                        cRqSetDepNoCacheChange;
                    end
                    else if(pRqEOC matches tagged Valid .m) begin
                        // append to pRq which must be the owner pRq
                        // since owner pRq is EOC, it should not have successor
                        doAssert(m == pOwner.mshrIdx && !pOwner.hasSucc,
                            "cRq hit on pRq and append to pRq, two pRq must be same"
                        );
                        // do appending & set cRq to Depend
                        pRqMshr.setSucc(pOwner.mshrIdx, Valid (n));
                        cRqMshr.setStateSlot(n, Depend, defaultValue);
                        // change hasSucc bit from 0 to 1 in RAM
                        pipeline.deqWrite(Invalid, RamData {
                            info: CacheInfo {
                                tag: ram.info.tag,
                                cs: ram.info.cs,
                                dir: ram.info.dir,
                                owner: PRq (PRqOwner {
                                    mshrIdx: pOwner.mshrIdx,
                                    hasSucc: True // set this
                                })
                            },
                            line: ram.line
                        });
                    end
                end
                else begin
                    // this way is rendered as a condidate for replacement
                    // so pRq has no successor
                    doAssert(!pOwner.hasSucc, "pRq cannot have successor");
                    // we first check for dep chain
                    // pRq cannot be in dep chain now
                    // because there are only two cases that pRq will be in chain
                    // 1. pRq blocked by cRq that has enough cs: this cRq should then tag match hit
                    // 2. pRq is owning the cache line: this cRq should then also tag match hit
                    doAssert(!isValid(pRqEOC), "pRq cannot be in dep chain");
                    // so only check if there is cRq in chain
                    if(cRqEOC matches tagged Valid .m) begin
                        $display("%t Int %m pipelineResp: cRq: own by pRq: tag mismatch, depend on cRq");
                        cRqMshr.setSucc(m, Valid (CRq (n)));
                        cRqSetDepNoCacheChange;
                    end
                    else begin
                        // no dep, replace this line & drop pRq
                        // resp for pRq will be the replacement resp
                        pRqSlotT pSlot <- pRqMshr.getSlot(pOwner.mshrIdx);
                        pRqMshr.setStateSlot(pOwner.mshrIdx, Empty, ?);
                        // get dir pend (utilize dirPend in pSlot to reduce rq to children)
                        // XXX to maintain the invariant that only 1 pRq to an addr in the child cache
                        // if pRq has sent req to some children, we must wait them to finish first
                        // we cannot send new ones now (e.g. pRq has sent down to S, we can't send down to S)
                        // XXX during replacement, when cRs has made dirPend to be invalid
                        // we have to check dir and recompute dirPend to make sure dir is I
                        function DirPend getSingleDirPend(Integer i);
                            if(pSlot.dirPend[i] matches tagged Waiting .st) begin
                                return pSlot.dirPend[i];
                            end
                            else begin
                                return ram.info.dir[i] == I ? Invalid : ToSend (I);
                            end
                        endfunction
                        Vector#(childNum, DirPend) dirPend = map(getSingleDirPend, genVector);
                        $display("%t Int %m pipelineResp: cRq: own by pRq: tag mismatch, replace pRq: ", $time,
                            fshow(pSlot), " ; ",
                            fshow(dirPend)
                        );
                        // since pRq is still owning the line, dir cannot be all I
                        // and dirPend must not be all invalid
                        doAssert(dirPend != replicate(Invalid) && ram.info.dir != replicate(I),
                            "pRq still valid, some children still need downgrade"
                        );
                        // do replacement
                        cRqStartReplace(dirPend);
                    end
                end
            end
            else begin
                // cache has no owner, cRq must just go through tag match
                // for the same reason, pRq cannot be in dep chain
                // (otherwise, this cRq must hit on a cRq or pRq)
                doAssert(cState == Init && !isValid(pRqEOC), "pRq cannot be in dep chain");
                // check for cRqEOC to append to dependency chain
                if(cRqEOC matches tagged Valid .m) begin
                    $display("%t Int %m pipelineResp: cRq: no owner, depend on cRq", $time);
                    cRqMshr.setSucc(m, Valid (CRq (n)));
                    cRqSetDepNoCacheChange;
                end
                else begin
                    // normal processing
                    if(ram.info.cs == I || ram.info.tag == getTag(cRq.addr)) begin
                        // No Replacement necessary, check dir
                        Vector#(childNum, DirPend) dirPend = getDirPendNonCompat;
                        doAssert((dirPend == replicate(Invalid)) == dirAllCompat(cRq),
                            "dirPend same as dir compat"
                        );
                        // check Hit or not
                        if(ram.info.cs >= cRq.toState && dirAllCompat(cRq)) begin
                            $display("%t Int %m pipelineResp: cRq: no owner, hit", $time);
                            doAssert(ram.info.tag == getTag(cRq.addr), "tag should match");
                            cRqHit(n, cRq);
                        end
                        else begin
                            $display("%t Int %m pipelineResp: cRq: no owner, miss no replace: ", $time, fshow(dirPend));
                            doAssert(ram.info.cs < cRq.toState || dirPend != replicate(Invalid),
                                "must be waiting for some state change"
                            );
                            cRqMissNoReplacement(dirPend);
                        end
                    end
                    else begin
                        // need replacement check dir
                        Vector#(childNum, DirPend) dirPend = getDirPendGtSt(I);
                        doAssert((dirPend == replicate(Invalid)) == (ram.info.dir == replicate(I)),
                            "dirPend same as dir compat"
                        );
                        if(ram.info.dir == replicate(I)) begin
                            $display("%t Int %m pipelineResp: cRq: no owner, miss with replace: evict", $time);
                            // directly evict the line
                            cRqEvict(n, cRq);
                        end
                        else begin
                            $display("%t Int %m pipelineResp: cRq: no owner, miss with replace: do replace", $time, 
                                fshow(dirPend)
                            );
                            // wait children to downgrade
                            cRqStartReplace(dirPend);
                        end
                    end
                end
            end
        end
        else if(pipeOut.cmd matches tagged IntPRq .n) begin
            pRqFromPT pRq <- pRqMshr.getRq(n);
            PRqState pState <- pRqMshr.getState(n);

            // find end of dependency chain
            Maybe#(cRqIndexT) cRqEOC <- cRqMshr.searchEndOfChain(pRq.addr);
            Maybe#(pRqIndexT) _pRqEOC <- pRqMshr.searchEndOfChain(pRq.addr);
            $display("%t Int %m pipelineResp: pRq: ", $time,
                fshow(n), " ; ", 
                fshow(pRq), " ; ",
                fshow(cRqEOC), " ; ",
                fshow(_pRqEOC)
            );
            // since there is always at most 1 pRq for the same addr
            // so if some pRq is end of chain, it must be this pRq (i.e. swapped in)
            doAssert(_pRqEOC == Invalid || _pRqEOC == Valid (n),
                "either no pRq in chain, no is the pRq itself"
            );
            // not two EOC
            doAssert(!(isValid(cRqEOC) && isValid(_pRqEOC)),
                "at most 1 EOC"
            );

            if(pipeOut.pRqMiss || ram.info.cs <= pRq.toState || ram.info.tag != getTag(pRq.addr)) begin
                $display("%t Int %m pipelineResp: pRq: drop", $time);
                // pRq can be directly dropped
                // must go through tag match, and has no successor
                // (swapped in pRq must need processing)
                Maybe#(cRqIndexT) succ <- pRqMshr.getSucc(n);
                doAssert(pState == Init && succ == Invalid,
                    "pRq must go through pipe and has no successor"
                );
                // drop pRq (RAM should not be changed)
                pRqMshr.setStateSlot(n, Empty, ?);
                pipeline.deqWrite(Invalid, pipeOut.ram);
                // sanity check: when pRqMiss = false, there is only one case
                // (so ram.info.tag != getTag(pRq.addr) is useless...)
                if(!pipeOut.pRqMiss) begin
                    doAssert(pRq.toState == S && ram.info.cs == S && ram.info.tag == getTag(pRq.addr),
                        ("pRqMiss deasserted, must be down to S")
                    );
                end
            end
            else begin
                // this pRq needs processing
                // function to process a pRq, which needs to wait for children to downgrade
                function Action pRqStartProcess(Vector#(childNum, DirPend) dirPend, Bool hasSucc);
                action
                    doAssert(dirPend != replicate(Invalid), "should wait for some child");
                    pRqMshr.setStateSlot(n, Processing, PRqSlot {
                        way: pipeOut.way,
                        dirPend: dirPend,
                        data: Invalid
                    });
                    pipeline.deqWrite(Invalid, RamData {
                        info: CacheInfo {
                            tag: ram.info.tag,
                            cs: ram.info.cs,
                            dir: ram.info.dir,
                            owner: PRq (PRqOwner {
                                mshrIdx: n,
                                hasSucc: hasSucc
                            })
                        },
                        line: ram.line
                    });
                endaction
                endfunction

                // inspect cache owner
                if(ram.info.owner matches tagged CRq .cOwner) begin
                    // pRq just go throug pipeline
                    doAssert(pState == Init, "pRq go through pipeline");
                    // check whether RAM has enough permission to serve cRq
                    cRqFromCT cRq <- cRqMshr.getRq(cOwner.mshrIdx);
                    CRqState cState <- cRqMshr.getState(cOwner.mshrIdx);
                    cRqSlotT cSlot <- cRqMshr.getSlot(cOwner.mshrIdx);
                    $display("%t Int %m pipelineResp: pRq: own by cRq: ", $time,
                        fshow(cOwner), " ; ",
                        fshow(cRq), " ; ",
                        fshow(cState), " ; ",
                        fshow(cSlot)
                    );
                    if(ram.info.cs >= cRq.toState) begin
                        $display("%t Int %m pipelineResp: pRq: own by cRq: depend on cRq", $time);
                        // cRq can be served, must be the following case
                        doAssert(ram.info.cs == M && cState == WaitSt,
                            "pRq blocked by cRq which has enough permission"
                        );
                        // make pRq depend on cRq, keep RAM unchanged
                        doAssert(isValid(cRqEOC), "cRqEOC should exist");
                        cRqMshr.setSucc(fromMaybe(?, cRqEOC), Valid (PRq (n)));
                        pRqMshr.setStateSlot(n, Depend, defaultValue);
                        pipeline.deqWrite(Invalid, ram);
                    end
                    else begin
                        // pRq should over take cRq, must be the following case
                        doAssert(ram.info.cs == S && cRq.toState == M && pRq.toState == I && cState == WaitSt && cSlot.waitP,
                            ("pRq overtakes CRq")
                        );
                        // cRq.dirPend cannot be in Waiting S
                        // (only ToSend (I), Waiting (I), Invalid)
                        function Bool assertCRqDirPend(DirPend dp);
                            return dp == Invalid || dp == Waiting (I) || dp == ToSend (I);
                        endfunction
                        doAssert(all(assertCRqDirPend, cSlot.dirPend),
                            "cRq dirPend can only be Invalid, Waiting I, ToSend I"
                        );
                        // calculate pRq.dirPend based on cRq.dirPend
                        // (don't send duplicate req)
                        // XXX since cRq only send down to I req (different from replacement case)
                        // there is no need to recompute dirPend after cRs makes it Invalid
                        function DirPend getSingleDirPend(Integer i);
                            if(cSlot.dirPend[i] == Waiting (I)) begin
                                return cSlot.dirPend[i];
                            end
                            else begin
                                return ram.info.dir[i] == I ? Invalid : ToSend (I);
                            end
                        endfunction
                        Vector#(childNum, DirPend) dirPend = map(getSingleDirPend, genVector);
                        $display("%t Int %m pipelineResp: pRq: overtake cRq: ", $time,
                            fshow(dirPend)
                        );
                        doAssert((dirPend == replicate(Invalid)) == (ram.info.dir == replicate(I)),
                            "dirPend same as dir all I"
                        );
                        // process pRq
                        if(ram.info.dir == replicate(I)) begin
                            $display("%t Int %m pipelineResp: pRq: overtake cRq: pRq done", $time);
                            // pRq can be done
                            // send resp to indexQ, change MSHR & pipeline & RAM
                            rsToPIndexQ.enq(PRq (n));
                            pRqMshr.setStateSlot(n, Done, PRqSlot {
                                way: ?,
                                dirPend: ?,
                                data: Invalid // only data is valid
                            });
                            pipeline.deqWrite(Invalid, RamData {
                                info: CacheInfo {
                                    tag: ram.info.tag, // keep tag the same (for sake of cRq)
                                    cs: I, // downgraded to I
                                    dir: replicate(I),
                                    owner: ram.info.owner // keep owner to cRq
                                },
                                line: ? // data becomes useless
                            });
                            // update cRq bookkeeping
                            cRqMshr.setStateSlot(cOwner.mshrIdx, WaitSt, CRqSlot {
                                way: pipeOut.way,
                                cs: I, // update cs (actually useless, it's OK for rqToP.fromState is S)
                                waitP: True,
                                repTag: ?,
                                dirPend: replicate(Invalid),
                                data: Invalid
                            });
                        end
                        else begin
                            $display("%t Int %m pipelineResp: pRq: overtake cRq: pRq process", $time);
                            // pRq should wait for children to downgrade
                            // pRq becomes cache line owner
                            // and cRq becomes dependent on pRq, so pRq has successor
                            pRqMshr.setSucc(n, Valid (cOwner.mshrIdx));
                            pRqStartProcess(dirPend, True);
                            // update cRq bookkeeping & set to depend
                            cRqMshr.setStateSlot(cOwner.mshrIdx, Depend, CRqSlot {
                                way: pipeOut.way,
                                cs: I, // update cs (actually useless, it's OK for rqToP.fromState is S)
                                waitP: True, // this bit must be set
                                repTag: ?,
                                dirPend: replicate(Invalid),
                                data: Invalid
                            });
                        end
                    end
                end
                else if(ram.info.owner matches tagged PRq .pOwner) begin
                    // must be swapped in, so pOwner is pRq itself
                    doAssert(pState == Depend && n == pOwner.mshrIdx,
                        "pRq hit on pRq which must be itself"
                    );
                    // check dir
                    Vector#(childNum, DirPend) dirPend = getDirPendGtSt(pRq.toState);
                    $display("%t Int %m pipelineResp: pRq: own by pRq", $time,
                        fshow(pOwner), " ; ",
                        fshow(dirPend)
                    );
                    doAssert((dirPend == replicate(Invalid)) == dirAllLeSt(pRq.toState),
                        "dirPend same as dir all le"
                    );
                    if(dirAllLeSt(pRq.toState)) begin
                        // pRq can be done
                        $display("%t Int %m pipelineResp: pRq: own by pRq: done", $time);
                        pRqDone(n, pRq);
                    end
                    else begin
                        // pRq wait for children to downgrade
                        Maybe#(cRqIndexT) succ <- pRqMshr.getSucc(n);
                        $display("%t Int %m pipelineResp: pRq: own by pRq: process: ", $time, fshow(succ));
                        pRqStartProcess(dirPend, isValid(succ));
                    end
                end
                else begin
                    // no cache owner, pRq go through pipeline, process it
                    doAssert(pState == Init && ram.info.owner == Invalid,
                        "should not have owner"
                    );
                    // check dir
                    Vector#(childNum, DirPend) dirPend = getDirPendGtSt(pRq.toState);
                    $display("%t Int %m pipelineResp: pRq: no owner", $time,
                        fshow(dirPend)
                    );
                    doAssert((dirPend == replicate(Invalid)) == dirAllLeSt(pRq.toState),
                        "dirPend same as dir all le"
                    );
                    if(dirAllLeSt(pRq.toState)) begin
                        $display("%t Int %m pipelineResp: pRq: no owner: done", $time);
                        // pRq can be done
                        pRqDone(n, pRq);
                    end
                    else begin
                        $display("%t Int %m pipelineResp: pRq: no owner: process", $time);
                        // pRq wait for children to downgrade
                        // cannot have successor, because just go through pipeline
                        pRqStartProcess(dirPend, False);
                    end
                end
            end
        end
        else if(pipeOut.cmd matches tagged IntCRs .child) begin
            $display("%t Int %m pipelineResp: cRs: ", $time, fshow(child));
            // cRs must hit on valid line
            doAssert(ram.info.cs > I, "cRs must hit on valid line");
            // check cache owner
            if(ram.info.owner matches tagged CRq .cOwner) begin
                // cRs hit on cRq, there are two cases
                // 1. cRq is replacing, this cRs is useful
                // 2. cRq is in WaitSt, this cRs could be irrelavent..
                // (WaitNewTag state cannot recv any cRs)
                cRqFromCT cRq <- cRqMshr.getRq(cOwner.mshrIdx);
                CRqState cState <- cRqMshr.getState(cOwner.mshrIdx);
                cRqSlotT cSlot <- cRqMshr.getSlot(cOwner.mshrIdx);
                $display("%t Int %m pipelineResp: cRs: own by cRq: ", $time,
                    fshow(cOwner), " ; ",
                    fshow(cRq), " ; ",
                    fshow(cState), " ; ",
                    fshow(cSlot)
                );
                doAssert(cSlot.way == pipeOut.way, "cRs way must match MSHR slot");
                doAssert(cState == WaitSt || cState == WaitOldTag,
                    "cRq has only two possible states"
                );
                // check replacing bit to distinguish two cases
                if(cOwner.replacing) begin
                    doAssert(cState == WaitOldTag, "cRq replacing");
                    doAssert(!cSlot.waitP && !isValid(cSlot.data),
                        "cannot req parent and has no data"
                    );
                    doAssert(cSlot.repTag == ram.info.tag, "replacing tag should match");
                    // update dirPend
                    // XXX it's possible that we recv down to S CRs (initiated by replaced pRq)
                    // in this case, we need to set dirPend to ToSend I
                    Vector#(childNum, DirPend) newDirPend = cSlot.dirPend;
                    if(ram.info.dir[child] == I) begin
                        newDirPend[child] = Invalid; // this child is invalidated, done
                    end
                    else begin
                        doAssert(ram.info.dir[child] == S, "must down to S");
                        case(cSlot.dirPend[child]) matches
                            tagged Waiting .st: begin
                                if(st == I) begin
                                    newDirPend[child] = Waiting (I); // keep waiting
                                end
                                else begin 
                                    // waiting S bit is reset
                                    // (caused by replacing line owned by pRq)
                                    doAssert(st == S, "must be waiting for S");
                                    // but we need to resend invalidation
                                    newDirPend[child] = ToSend (I);
                                end
                            end
                            default: begin
                                doAssert(cSlot.dirPend[child] == ToSend (I), "must be ToSend I");
                                newDirPend[child] = ToSend (I); // need invalidation
                            end
                        endcase
                    end
                    $display("%t Int %m pipelineResp: cRs: own by cRq: replacing: ", $time,
                        fshow(newDirPend)
                    );
                    doAssert((newDirPend == replicate(Invalid)) == (ram.info.dir == replicate(I)),
                        "dirPend same as dir all I"
                    );
                    // check whether replacement is done
                    if(ram.info.dir == replicate(I)) begin
                        $display("%t Int %m pipelineResp: cRs: own by cRq: replacing: evict", $time);
                        cRqEvict(cOwner.mshrIdx, cRq);
                    end
                    else begin
                        $display("%t Int %m pipelineResp: cRs: own by cRq: replacing: still wait", $time);
                        // replacement is still ongoing, update dirPend
                        // update cRq MSHR
                        cRqMshr.setStateSlot(cOwner.mshrIdx, WaitOldTag, CRqSlot {
                            way: cSlot.way,
                            cs: ram.info.cs,
                            repTag: cSlot.repTag,
                            waitP: cSlot.waitP,
                            dirPend: newDirPend,
                            data: Invalid
                        });
                        // deq cRs & write ram
                        pipeline.deqWrite(Invalid, ram);
                    end
                end
                else begin
                    doAssert(cState == WaitSt, "must be waiting child/parent state");
                    doAssert(ram.info.tag == getTag(cRq.addr), "tag should match");
                    doAssert(!isValid(cSlot.data), "no data");
                    // update dirPend XXX cSlot.dirPend is already accurate
                    // no need to recompute it (as in case of replacement)
                    // just reset bits
                    Vector#(childNum, DirPend) newDirPend = cSlot.dirPend;
                    case(cSlot.dirPend[child]) matches
                        tagged ToSend .st: begin
                            if(st >= ram.info.dir[child]) begin
                                newDirPend[child] = Invalid;
                            end
                        end
                        tagged Waiting .st: begin
                            if(st >= ram.info.dir[child]) begin
                                newDirPend[child] = Invalid;
                            end
                        end
                    endcase
                    $display("%t Int %m pipelineResp: cRs: own by cRq: WaitSt: ", $time,
                        fshow(newDirPend)
                    );
                    doAssert((newDirPend == replicate(Invalid)) == dirAllCompat(cRq),
                        "dirPend same as dir compat"
                    );
                    // check Hit or not
                    if(ram.info.cs >= cRq.toState && dirAllCompat(cRq)) begin
                        $display("%t Int %m pipelineResp: cRs: own by cRq: WaitSt: cRq hit", $time);
                        // cRq done
                        cRqHit(cOwner.mshrIdx, cRq);
                    end
                    else begin
                        $display("%t Int %m pipelineResp: cRs: own by cRq: WaitSt: cRq still wait", $time);
                        // still waiting for correct state, update dir
                        doAssert(newDirPend != replicate(Invalid) || ram.info.cs < cRq.toState,
                            "must wait for children or parent"
                        );
                        // update cRq MSHR
                        cRqMshr.setStateSlot(cOwner.mshrIdx, WaitSt, CRqSlot {
                            way: cSlot.way,
                            cs: ram.info.cs,
                            repTag: cSlot.repTag,
                            waitP: cSlot.waitP,
                            dirPend: newDirPend,
                            data: Invalid
                        });
                        // deq cRs & write ram
                        pipeline.deqWrite(Invalid, ram);
                    end
                end
            end
            else if(ram.info.owner matches tagged PRq .pOwner) begin
                pRqFromPT pRq <- pRqMshr.getRq(pOwner.mshrIdx);
                pRqSlotT pSlot <- pRqMshr.getSlot(pOwner.mshrIdx);
                PRqState pState <- pRqMshr.getState(pOwner.mshrIdx);
                doAssert(ram.info.cs > pRq.toState && ram.info.tag == getTag(pRq.addr),
                    "pRq should match addr and proper state"
                );
                doAssert(pSlot.way == pipeOut.way, "way should match");
                doAssert(!isValid(pSlot.data), "no data");
                doAssert(pState == Processing, "pRq must be Processing");
                // update dirPend XXX pSlot.dirPend is already accurate
                // no need to recompute, just reset bits
                Vector#(childNum, DirPend) newDirPend = pSlot.dirPend;
                case(pSlot.dirPend[child]) matches
                    tagged ToSend .st: begin
                        if(st >= ram.info.dir[child]) begin
                            newDirPend[child] = Invalid;
                        end
                    end
                    tagged Waiting .st: begin
                        if(st >= ram.info.dir[child]) begin
                            newDirPend[child] = Invalid;
                        end
                    end
                endcase
                $display("%t Int %m pipelineResp: cRs: own by pRq: ", $time,
                    fshow(pOwner), " ; ",
                    fshow(pRq), " ; ",
                    fshow(pState), " ; ",
                    fshow(pSlot), " ; ",
                    fshow(newDirPend)
                );
                doAssert((newDirPend == replicate(Invalid)) == dirAllLeSt(pRq.toState),
                    "dirPend same as dir all Le"
                );
                // check dir states
                if(dirAllLeSt(pRq.toState)) begin
                    $display("%t Int %m pipelineResp: cRs: own by pRq: done", $time);
                    // pRq can be done
                    pRqDone(pOwner.mshrIdx, pRq);
                end
                else begin
                    $display("%t Int %m pipelineResp: cRs: own by pRq: still wait", $time);
                    // pRq keep waiting for children, update pRq MSHR
                    pRqMshr.setStateSlot(pOwner.mshrIdx, Processing, PRqSlot {
                        way: pSlot.way,
                        dirPend: newDirPend,
                        data: Invalid
                    });
                    // deq cRs & write ram
                    pipeline.deqWrite(Invalid, ram);
                end
            end
            else begin
                $display("%t Int %m pipelineResp: cRs: no owner", $time);
                // no owner, just deq cRs & write ram
                doAssert(ram.info.owner == Invalid, "no owner");
                pipeline.deqWrite(Invalid, ram);
            end
        end
        else if(pipeOut.cmd == IntPRs) begin
            if(ram.info.owner matches tagged CRq .cOwner) begin
                cRqFromCT cRq <- cRqMshr.getRq(cOwner.mshrIdx);
                CRqState cState <- cRqMshr.getState(cOwner.mshrIdx);
                cRqSlotT cSlot <- cRqMshr.getSlot(cOwner.mshrIdx);
                $display("%t Int %m pipelineResp: pRs: ", $time,
                    fshow(cOwner), " ; ",
                    fshow(cRq) , " ; ",
                    fshow(cState), " ; ",
                    fshow(cSlot)
                );
                doAssert(ram.info.cs >= cRq.toState && ram.info.tag == getTag(cRq.addr),
                    "pRs must be a hit"
                );
                doAssert(!cOwner.replacing, "cannot be replacing");
                doAssert(cState == WaitSt, "pRs hits on cRq in WaitSt");
                doAssert(cSlot.waitP, "cRq should be waiting for pRs");
                doAssert(!isValid(cSlot.data), "no data");
                doAssert(cSlot.way == pipeOut.way, "way should match");
                doAssert((cSlot.dirPend == replicate(Invalid)) == dirAllCompat(cRq),
                    "dirPend same as dir all compat"
                );
                // check dir states
                if(dirAllCompat(cRq)) begin
                    $display("%t Int %m pipelineResp: pRs: cRq hit", $time);
                    // cRq done
                    cRqHit(cOwner.mshrIdx, cRq);
                end
                else begin
                    $display("%t Int %m pipelineResp: pRs: cRq still wait", $time);
                    // still wait for children, reset waitP bits
                    cRqMshr.setStateSlot(cOwner.mshrIdx, WaitSt, CRqSlot {
                        way: cSlot.way,
                        cs: ram.info.cs,
                        repTag: cSlot.repTag,
                        waitP: False, // reset bit
                        dirPend: cSlot.dirPend,
                        data: Invalid
                    });
                    // deq pipe & write ram
                    pipeline.deqWrite(Invalid, ram);
                end
            end
            else begin
                doAssert(False, ("pRs owner must match some cRq"));
            end
        end
        else begin
            doAssert(False, "unknown IntCmd");
        end
    endrule

    interface rsToPGet = toFifoDeq(rsToPQ);
    interface rqToPGet = toFifoDeq(rqToPQ);
    interface fromPPut = toFifoEnq(fromPQ);
    interface toCGet = toFifoDeq(toCQ);
    interface rqFromCPut = toFifoEnq(rqFromCQ);
    interface rsFromCPut = toFifoEnq(rsFromCQ);
endmodule
