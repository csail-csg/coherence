
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
import DefaultValue::*;
import GetPut::*;
import BRAMCore::*;
import FIFO::*;
import Fifo::*;
import FShow::*;
import CacheUtils::*;
import CCTypes::*;
import Types::*;
import CRqMshr::*;

// XXX we need to maintain the invariant that 
// at most 1 pRq sent to a child for an addr
// and we have to wait for resp before sending anthoer one

// In future, it would be hard to parallelize sendRqToC and pipelineResp
// because sendRqToC must see updates to dirPend made in pipelineResp
// or we simply stall sendRqToC

interface DirBank#(
    numeric type lgBankNum,
    numeric type childNum,
    numeric type indexSz, // fully mapped dir, no tag
    numeric type cRqNum,
    type cRqIdT
);
    interface FifoEnq#(CRqMsg#(cRqIdT, Bit#(TLog#(childNum)))) rqFromCPut;
    interface FifoEnq#(CRsMsg#(Bit#(TLog#(childNum)))) rsFromCPut;
    interface FifoDeq#(PRqRsMsg#(cRqIdT, Bit#(TLog#(childNum)))) toCGet;

    interface MemFifoClient#(Bit#(TLog#(cRqNum)), void) to_mem;
endinterface

typedef union tagged {
    void Invalid;
    cRqIndexT DirCRq;
    Addr DirCRs;
    cRqIndexT DirMRs;
} RamOwner#(type cRqIndexT) deriving(Bits, Eq, FShow);

module mkDirBank#(
    module#(CRqMshr#(childNum, cRqNum, Bit#(0), Bit#(0), Bit#(0), cRqFromCT)) mkDirCRqMshr
)(
    DirBank#(lgBankNum, childNum, indexSz, cRqNum, cRqIdT)
) provisos(
    Alias#(dirT, Vector#(childNum, Msi)),
    Alias#(indexT, Bit#(indexSz)),
    Alias#(cRqIndexT, Bit#(TLog#(cRqNum))),
    Alias#(childT, Bit#(TLog#(childNum))),
    Alias#(cRqFromCT, CRqMsg#(cRqIdT, childT)),
    Alias#(cRsFromCT, CRsMsg#(childT)),
    Alias#(pRqRsToCT, PRqRsMsg#(cRqIdT, childT)),
    Alias#(memRsT, MemRsMsg#(cRqIndexT, void)),
    Alias#(toMemT, ToMemMsg#(cRqIndexT, void)),
    Alias#(cRqSlotT, CRqSlot#(childNum, Bit#(0), Bit#(0))),
    Add#(indexSz, a__, AddrSz),
    Alias#(cRqIdT, Bit#(_cRqIdSz))
);

    function indexT getIndex(Addr a);
        return truncate(a >> (valueOf(lgBankNum) + valueOf(LgLineSzBytes)));
    endfunction

    Fifo#(2, cRqFromCT) rqFromCQ <- mkCFFifo;

    Fifo#(2, cRsFromCT) rsFromCQ <- mkCFFifo;

    Fifo#(2, pRqRsToCT) toCQ <- mkCFFifo;

    Fifo#(2, toMemT) toMQ <- mkCFFifo;

    Fifo#(2, memRsT) rsFromMQ <- mkCFFifo;

    FIFO#(cRqIndexT) rqToMIndexQ <- mkSizedFIFO(valueOf(cRqNum)); // index of Ld req to memory

    Fifo#(cRqNum, cRqIndexT) rsToCIndexQ <- mkCFFifo;

    // dir bram 
    BRAM_PORT#(indexT, dirT) dirRam <- mkBRAMCore1(valueOf(TExp#(indexSz)), False);
    Reg#(RamOwner#(cRqIndexT)) ramOwner <- mkReg(Invalid);

    // MSHR for cRq
    CRqMshr#(childNum, cRqNum, Bit#(0), Bit#(0), Bit#(0), cRqFromCT) mshr <- mkDirCRqMshr;

    Reg#(Bool) initDone <- mkReg(False);
    Reg#(indexT) initIdx <- mkReg(0);

    rule doInit(!initDone);
        dirRam.put(True, initIdx, replicate(I));
        if(initIdx == maxBound) begin
            $display("%t MemDir InitDone", $time);
            initDone <= True;
        end
        else begin
            initIdx <= initIdx + 1;
        end
    endrule

    // XXX when reset MSHR entry to Empty
    // we need to reset succ field to invalid
    // thus, we don't need to set succ field when Init a MSHR slot

    rule cRqTransfer(initDone && ramOwner == Invalid);
        rqFromCQ.deq;
        cRqFromCT cRq = rqFromCQ.first;
        // get empty slot: XXX succ field is already INVALID
        Maybe#(cRqIndexT) emptySlot = mshr.getEmptySlot;
        check(isValid(emptySlot));
        cRqIndexT n = fromMaybe(?, emptySlot);
        mshr.setRq(n, cRq);
        // check for dependency chain here
        // we check early to avoid dir access latency (could be long)
        // fairness is achieved because we allow a cRq which is not added
        // to chain to access dir
        // since dir access is blocking, there is no cRq in Init state now
        Maybe#(cRqIndexT) eoc <- mshr.searchEndOfChain(cRq.addr);
        if(eoc matches tagged Valid .m) begin
            // add to chain
            mshr.setStateSlot(n, Depend, defaultValue);
            mshr.setSucc(m, Valid (CRq (n)));
            $display("%t Dir %m cRqTransfer: add to chain: ", $time,
                fshow(n), " ; ",
                fshow(cRq), " ; ",
                fshow(m)
            );
        end
        else begin
            mshr.setStateSlot(n, Init, defaultValue);
            // read dir
            dirRam.put(False, getIndex(cRq.addr), ?);
            ramOwner <= DirCRq (n);
            $display("%t Dir %m cRqTransfer: access dir: ", $time,
                fshow(n), " ; ",
                fshow(cRq)
            );
        end
    endrule

    // write memory happens atomically with read dir
    // this prevents cRs from locking the dirRam due to full toMQ
    // this avoids deadlock, because mRs always drain and toMQ always drain
    rule cRsTransfer(initDone && rsFromCQ.notEmpty && ramOwner == Invalid);
        // deq FIFO later
        cRsFromCT cRs = rsFromCQ.first;
        // read dir
        dirRam.put(False, getIndex(cRs.addr), ?);
        ramOwner <= DirCRs (cRs.addr);
        // write memory
        if(cRs.data matches tagged Valid .line) begin
            toMQ.enq(Wb (WbMemRs {
                addr: cRs.addr,
                data: line
            }));
        end
        $display("%t Dir %m cRsTransfer: ", $time, fshow(cRs));
    endrule

    rule mRsTransfer(initDone && rsFromMQ.notEmpty && ramOwner == Invalid);
        // deq FIFO later
        memRsT mRs = rsFromMQ.first;
        cRqFromCT cRq <- mshr.getRq(mRs.id);
        // read dir
        dirRam.put(False, getIndex(cRq.addr), ?);
        ramOwner <= DirMRs (mRs.id);
        $display("%t Dir %m mRsTransfer: ", $time, fshow(mRs), " ; ", fshow(cRq));
    endrule

    rule sendRqToM(initDone);
        // req memory data
        rqToMIndexQ.deq;
        cRqIndexT n = rqToMIndexQ.first;
        cRqFromCT cRq <- mshr.getRq(n);
        toMQ.enq(Ld (LdMemRq {
            addr: cRq.addr,
            child: ?,
            id: n
        }));
        $display("%t Dir %m sendRqToM: ", $time, fshow(n), " ; ", fshow(cRq.addr));
    endrule

    rule sendRsToC(initDone && rsToCIndexQ.notEmpty);
        // send upgrade resp to child
        rsToCIndexQ.deq;
        cRqIndexT n = rsToCIndexQ.first;
        cRqFromCT cRq <- mshr.getRq(n);
        cRqSlotT cSlot <- mshr.getSlot(n);
        pRqRsToCT resp = PRs (PRsMsg {
            addr: cRq.addr,
            toState: cRq.toState,
            child: cRq.child,
            data: cSlot.data,
            id: cRq.id
        });
        toCQ.enq(resp);
        // reset MSHR entry
        mshr.setStateSlot(n, Empty, defaultValue);
        mshr.setSucc(n, Invalid); // XXX reset succ here
        $display("%t Dir %m sendRsToC: ", $time, 
            fshow(n), " ; ", 
            fshow(cRq), " ; ", 
            fshow(resp)
        );
    endrule

    // round robin select cRq to downgrade child
    // but downgrade must wait for all upgrade resp
    Reg#(cRqIndexT) whichCRq <- mkReg(0);
    rule sendRqToC(initDone && !rsToCIndexQ.notEmpty);
        Maybe#(cRqIndexT) cRqNeedDown = mshr.searchNeedRqChild(Valid (whichCRq));
        // XXX must add this into guard
        // otherwise this rule will block dirResp rule from firing
        check(isValid(cRqNeedDown));

        cRqIndexT n = fromMaybe(?, cRqNeedDown);
        cRqFromCT cRq <- mshr.getRq(n);
        cRqSlotT cSlot <- mshr.getSlot(n);
        // find a child to downgrade
        function Bool needSend(DirPend dp);
            return dp matches tagged ToSend .s ? True : False;
        endfunction
        Maybe#(childT) childToDown = searchIndex(needSend, cSlot.dirPend);
        doAssert(isValid(childToDown), ("should have a child to downgrade"));
        childT child = fromMaybe(?, childToDown);
        if(cSlot.dirPend[child] matches tagged ToSend .s) begin
            // send downgrade req
            pRqRsToCT req = PRq (PRqMsg {
                addr: cRq.addr,
                toState: s,
                child: child
            });
            toCQ.enq(req);
            // change dirPend
            Vector#(childNum, DirPend) newDirPend = cSlot.dirPend;
            newDirPend[child] = Waiting (s);
            mshr.setStateSlot(n, WaitSt, CRqSlot {
                way: ?,
                cs: ?,
                repTag: ?,
                waitP: cSlot.waitP,
                dirPend: newDirPend,
                data: cSlot.data
            });
            $display("%t Dir %m sendRqToC: ", $time,
                fshow(n), " ; ",
                fshow(cSlot), " ; ",
                fshow(req)
            );
        end
        else begin
            doAssert(False, ("dirPend should be ToSend"));
        end
        // change round-robin
        whichCRq <= whichCRq == fromInteger(valueOf(cRqNum) - 1) ? 0 : whichCRq + 1;
    endrule

    rule dirResp(initDone && ramOwner != Invalid);
        dirT dir = dirRam.read;
        $display("%t Dir %m dirResp: ", $time, 
            fshow(ramOwner), " ; ",
            fshow(dir)
        );

        function Action respChild(cRqIndexT n, Addr addr, Maybe#(Line) data, dirT newDir);
        action
            dirRam.put(True, getIndex(addr), newDir);
            mshr.setStateSlot(n, Done, CRqSlot {
                way: ?,
                cs: ?,
                repTag: ?,
                waitP: False,
                dirPend: replicate(Invalid),
                data: data
            });
            rsToCIndexQ.enq(n);
            // wake up successor
            Maybe#(MshrIndex#(cRqIndexT, Bit#(0))) succ <- mshr.getSucc(n);
            if(succ matches tagged Valid (tagged CRq .m)) begin
                ramOwner <= DirCRq (m);
                // FIXME next cycle dirRam output should be newDir
                // according to Verilog source file BRAM1.v
            end
            else begin
                ramOwner <= Invalid;
            end
            $display("%t Dir %m dirResp: resp child:", $time,
                fshow(n), " ; ",
                fshow(addr), " ; ",
                fshow(data), " ; ",
                fshow(newDir), " ; ",
                fshow(succ)
            );
        endaction
        endfunction
            
        if(ramOwner matches tagged DirCRq .n) begin
            cRqFromCT cRq <- mshr.getRq(n);
            // since dependency chain is checked early
            // we don't need to check again
            
            // check whether child need data
            // we can determine now because cRq.fromState is either I or S
            // if it is I, then child need data
            // otherwise it is S. In this case, the only way to downgrade this child 
            // (to make it need data) is by a pRq sent from here,
            // so we must have seen the corresponding cRs, and dir should be I
            Bool childNeedData = dir[cRq.child] == I || cRq.fromState == I;
            // then check where is the latest data
            function Bool isMState(Msi s) = (s == M);
            Bool dataInChild = any(isMState, dir);
            // only req memory when child need data and latest data is not in child
            Bool reqMem = childNeedData && !dataInChild;
            if(reqMem) begin
                rqToMIndexQ.enq(n);
            end

            // check whether children needs downgrade
            function Vector#(childNum, DirPend) initDirPend;
                function DirPend initPend(childT i);
                    if(i == cRq.child) begin
                        return dir[i] <= cRq.fromState ? Invalid : Waiting (cRq.fromState);
                    end
                    else begin
                        Msi compatState = toCompat(cRq.toState);
                        return dir[i] <= compatState ? Invalid : ToSend (compatState);
                    end
                endfunction
                Vector#(childNum, childT) idxVec = genWith(fromInteger);
                return map(initPend, idxVec);
            endfunction
            Vector#(childNum, DirPend) dirPend = initDirPend;

            $display("%t Dir %m dirResp: cRq: ", $time,
                fshow(n), " ; ",
                fshow(cRq), " ; ",
                fshow(childNeedData), " ; ",
                fshow(dataInChild), " ; ",
                fshow(reqMem), " ; ",
                fshow(dirPend)
            );

            if(dirPend == replicate(Invalid) && !childNeedData) begin
                // directly resp without data
                dirT newDir = dir;
                newDir[cRq.child] = cRq.toState;
                respChild(n, cRq.addr, Invalid, newDir);
            end
            else begin
                // release ram, start waiting
                mshr.setStateSlot(n, WaitSt, CRqSlot {
                    way: ?,
                    cs: ?,
                    repTag: ?,
                    waitP: reqMem,
                    dirPend: dirPend,
                    data: Invalid
                });
                ramOwner <= Invalid;
            end
        end
        else if(ramOwner matches tagged DirCRs .addr) begin
            // deq & get cRs
            rsFromCQ.deq;
            cRsFromCT cRs = rsFromCQ.first;
            doAssert(isValid(cRs.data) == (dir[cRs.child] == M),
                ("only downgrade from M carries data")
            );
            // change dir
            dirT newDir = dir;
            newDir[cRs.child] = cRs.toState; // write dir a little bit later
            // memory has been written, so no need to do again
            $display("%t Dir %m dirResp: cRs: ", $time, fshow(cRs));
            // check cRq affected by this downgrade
            Maybe#(cRqIndexT) wakeUpCRq = mshr.searchWaitSt_MatchAddr(cRs.addr);
            if(wakeUpCRq matches tagged Valid .n) begin
                cRqFromCT cRq <- mshr.getRq(n);
                cRqSlotT cSlot <- mshr.getSlot(n);
                // change dir pend
                Vector#(childNum, DirPend) newDirPend = cSlot.dirPend;
                case(cSlot.dirPend[cRs.child]) matches
                    tagged ToSend .s: begin
                        if(s >= cRs.toState) begin
                            newDirPend[cRs.child] = Invalid;
                        end
                    end
                    tagged Waiting .s: begin
                        if(s >= cRs.toState) begin
                            newDirPend[cRs.child] = Invalid;
                        end
                    end
                endcase
                // get new data: if cRs has data, then cRq.child must need this data
                Maybe#(Line) newData = isValid(cRs.data) ? cRs.data : cSlot.data;
                $display("%t Dir %m dirResp: cRs: wake up cRq: ", $time,
                    fshow(n), " ; ",
                    fshow(cRq), " ; ",
                    fshow(cSlot), " ; ",
                    fshow(newDirPend), " ; ",
                    fshow(newData)
                );
                // update cRq states
                if(newDirPend == replicate(Invalid) && !cSlot.waitP) begin
                    // cRq done (data must have been received if child needs it)
                    doAssert(isValid(newData) == (newDir[cRq.child] == I),
                        ("data valid bit should be correct when waken up by cRs")
                    );
                    // change dir & resp
                    newDir[cRq.child] = cRq.toState;
                    respChild(n, cRq.addr, newData, newDir);
                end
                else begin
                    // just write dir & cRq will still wait
                    mshr.setStateSlot(n, WaitSt, CRqSlot {
                        way: ?,
                        cs: ?,
                        repTag: ?,
                        waitP: cSlot.waitP,
                        dirPend: newDirPend,
                        data: newData
                    });
                    dirRam.put(True, getIndex(cRs.addr), newDir);
                    ramOwner <= Invalid; // release ram
                end
            end
            else begin
                // no cRq affected, just write dir
                dirRam.put(True, getIndex(cRs.addr), newDir);
                ramOwner <= Invalid;
            end
        end
        else if(ramOwner matches tagged DirMRs .n) begin
            // deq & get mRs, cRq
            rsFromMQ.deq;
            memRsT mRs = rsFromMQ.first;
            cRqFromCT cRq <- mshr.getRq(n);
            cRqSlotT cSlot <- mshr.getSlot(n);
            doAssert(n == mRs.id, ("mRs.id is mshr index"));
            doAssert(cSlot.waitP, ("mRs only comes after req"));
            $display("%t Dir %m dirResp: mRs: ", $time,
                fshow(n), " ; ",
                fshow(mRs), " ; ",
                fshow(cRq), " ; ",
                fshow(cSlot)
            );
            // process cRq
            if(cSlot.dirPend == replicate(Invalid)) begin
                // resp child with data
                doAssert(dir[cRq.child] == I, ("child should need data when mRs comes"));
                dirT newDir = dir;
                newDir[cRq.child] = cRq.toState;
                respChild(n, cRq.addr, Valid (mRs.data), newDir);
            end
            else begin
                // just record data & reset wait bit & still wait for child
                mshr.setStateSlot(n, WaitSt, CRqSlot {
                    way: ?,
                    cs: ?,
                    repTag: ?,
                    waitP: False,
                    dirPend: cSlot.dirPend,
                    data: Valid (mRs.data)
                });
                ramOwner <= Invalid; // release ram
            end
        end
        else begin
            doAssert(False, ("impossible dirResp case"));
        end
    endrule

    interface rqFromCPut = toFifoEnq(rqFromCQ);
    interface rsFromCPut = toFifoEnq(rsFromCQ);
    interface toCGet = toFifoDeq(toCQ);

    interface MemFifoClient to_mem;
        interface toMGet = toFifoDeq(toMQ);
        interface rsFromMPut = toFifoEnq(rsFromMQ);
    endinterface
endmodule
