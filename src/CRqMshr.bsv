
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
import RegFile::*;
import FShow::*;
import Types::*;
import CCTypes::*;
import DefaultValue::*;

// MSHR dependency chain invariant:
// every cRq and pRq (for same addr) which has gone through pipeline once will be linked into the chain

// CRq MSHR entry state
typedef enum {
    Empty, 
    Init, 
    WaitOldTag, // doing replacement
    WaitNewTag, // waiting replacement resp to send (but tag in RAM is already updated)
    WaitSt, // wait pRs/cRs to come
    Done, // resp is in index FIFO
    Depend
} CRqState deriving(Bits, Eq, FShow);

// CRq info returned to outside
typedef struct {
    wayT way; // the way to occupy
    Msi cs; // current cache MSI, used in sending upgrade req to parent
    tagT repTag; // tag being replaced: only valid in WaitOld/NewTag states
    Bool waitP; // wait parent resp
    Vector#(childNum, DirPend) dirPend; // pending child downgrade
    Maybe#(Line) data; // data to resp parent or child
} CRqSlot#(numeric type childNum, type wayT, type tagT) deriving(Bits, Eq, FShow);

instance DefaultValue#(CRqSlot#(childNum, wayT, tagT));
    defaultValue = CRqSlot {
        way: ?,
        cs: ?,
        repTag: ?,
        waitP: False,
        dirPend: replicate(Invalid),
        data: Invalid
    };
endinstance

interface CRqMshr#(
    numeric type childNum, 
    numeric type cRqNum, 
    type pRqIndexT, 
    type wayT,
    type tagT,
    type reqT // child req type
);
    method Maybe#(Bit#(TLog#(cRqNum))) getEmptySlot;
    method ActionValue#(CRqState) getState(Bit#(TLog#(cRqNum)) n);
    method ActionValue#(reqT) getRq(Bit#(TLog#(cRqNum)) n);
    method Action setRq(Bit#(TLog#(cRqNum)) n, reqT rq);
    method Action setStateSlot(
        Bit#(TLog#(cRqNum)) n, CRqState state, 
        CRqSlot#(childNum, wayT, tagT) slot
    );
    method Action setSucc(
        Bit#(TLog#(cRqNum)) n, 
        Maybe#(MshrIndex#(Bit#(TLog#(cRqNum)), pRqIndexT)) succ
    );
    method ActionValue#(Maybe#(MshrIndex#(Bit#(TLog#(cRqNum)), pRqIndexT))) getSucc(Bit#(TLog#(cRqNum)) n);
    method ActionValue#(CRqSlot#(childNum, wayT, tagT)) getSlot(Bit#(TLog#(cRqNum)) n);
    // find existing cRq which has gone through pipeline, but not in Done state, and has not successor
    // i.e. search the end of dependency chain
    method ActionValue#(Maybe#(Bit#(TLog#(cRqNum)))) searchEndOfChain(Addr addr);
    // find cRq that needs to send req to child to downgrade
    // (either replacement, or incompatible children states)
    // we can pass in a suggested req idx (which will have priority)
    method Maybe#(Bit#(TLog#(cRqNum))) searchNeedRqChild(Maybe#(Bit#(TLog#(cRqNum))) suggestIdx);
    // find cRq in WaitSt that matches the addr
    // just for the case of MemDir
    method Maybe#(Bit#(TLog#(cRqNum))) searchWaitSt_MatchAddr(Addr addr);
endinterface

module mkCRqMshr#(
    function Addr getAddrFromReq(reqT r)
)(
    CRqMshr#(childNum, cRqNum, pRqIndexT, wayT, tagT, reqT)
) provisos (
    Alias#(cRqIndexT, Bit#(TLog#(cRqNum))),
    Alias#(slotT, CRqSlot#(childNum, wayT, tagT)),
    Alias#(mshrIndexT, MshrIndex#(cRqIndexT, pRqIndexT)),
    Alias#(pRqIndexT, Bit#(_pRqIndexSz)),
    Alias#(wayT, Bit#(_waySz)),
    Alias#(tagT, Bit#(_tagSz)),
    Bits#(reqT, _reqSz)
);
    // MSHR entry state
    Vector#(cRqNum, Reg#(CRqState)) stateVec <- replicateM(mkReg(Empty));
    // cRq req contents
    Vector#(cRqNum, Reg#(reqT)) reqVec <- replicateM(mkRegU);
    // summary bit of dirPend in each entry: asserted when some dirPend[i] = ToSend
    Vector#(cRqNum, Reg#(Bool)) needReqChildVec <- replicateM(mkReg(False));
    // cRq mshr slots
    RegFile#(cRqIndexT, slotT) slotFile <- mkRegFile(0, fromInteger(valueOf(cRqNum) - 1));
    // successor valid bit
    Vector#(cRqNum, Reg#(Bool)) succValidVec <- replicateM(mkReg(False));
    // successor MSHR index
    RegFile#(cRqIndexT, mshrIndexT) succFile <- mkRegFile(0, fromInteger(valueOf(cRqNum) - 1));

    method Maybe#(cRqIndexT) getEmptySlot;
        return searchIndex(\== (CRqState'(Empty)), readVector(stateVec));
    endmethod

    method Action setRq(cRqIndexT n, reqT r);
        reqVec[n] <= r;
    endmethod

    method ActionValue#(reqT) getRq(cRqIndexT n);
        return reqVec[n];
    endmethod

    method ActionValue#(CRqState) getState(cRqIndexT n);
        return stateVec[n];
    endmethod

    method Action setStateSlot(cRqIndexT n, CRqState state, slotT slot);
        stateVec[n] <= state;
        slotFile.upd(n, slot);
        // set dirPend summary bit
        needReqChildVec[n] <= getNeedReqChild(slot.dirPend);
    endmethod

    method Action setSucc(cRqIndexT n, Maybe#(mshrIndexT) succ);
        succValidVec[n] <= isValid(succ);
        succFile.upd(n, fromMaybe(?, succ));
    endmethod

    method ActionValue#(Maybe#(mshrIndexT)) getSucc(cRqIndexT n);
        if(succValidVec[n]) begin
            return Valid (succFile.sub(n));
        end
        else begin
            return Invalid;
        end
    endmethod

    method ActionValue#(slotT) getSlot(cRqIndexT n);
        return slotFile.sub(n);
    endmethod

    method ActionValue#(Maybe#(cRqIndexT)) searchEndOfChain(Addr addr);
        function Bool isEndOfChain(Integer i);
            // check entry i is end of chain or not
            let state = stateVec[i];
            Bool notDone = state != Done;
            Bool processedOnce = state != Empty && state != Init;
            Bool addrMatch = getLineAddr(getAddrFromReq(reqVec[i])) == getLineAddr(addr);
            Bool noSucc = !succValidVec[i];
            return notDone && processedOnce && addrMatch && noSucc;
        endfunction
        Vector#(cRqNum, Integer) idxVec = genVector;
        return searchIndex(isEndOfChain, idxVec);
    endmethod

    method Maybe#(cRqIndexT) searchNeedRqChild(Maybe#(cRqIndexT) suggestIdx);
        function Bool isNeedRqChild(cRqIndexT i);
            return (stateVec[i] == WaitOldTag || stateVec[i] == WaitSt) && needReqChildVec[i];
        endfunction
        if(suggestIdx matches tagged Valid .idx &&& isNeedRqChild(idx)) begin
            return suggestIdx;
        end
        else begin
            Vector#(cRqNum, cRqIndexT) idxVec = genWith(fromInteger);
            return searchIndex(isNeedRqChild, idxVec);
        end
    endmethod

    method Maybe#(cRqIndexT) searchWaitSt_MatchAddr(Addr addr);
        function Bool isWaitStMatchAddr(Integer i);
            Bool isWaitSt = stateVec[i] == WaitSt;
            Bool addrMatch = getLineAddr(getAddrFromReq(reqVec[i])) == getLineAddr(addr);
            return isWaitSt && addrMatch;
        endfunction
        Vector#(cRqNum, Integer) idxVec = genVector;
        return searchIndex(isWaitStMatchAddr, idxVec);
    endmethod
endmodule

