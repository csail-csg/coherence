
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

// PRq MSHR entry state
typedef enum {
    Empty, 
    Init, 
    Processing, 
    Done, 
    Depend
} PRqState deriving (Bits, Eq, FShow);

// PRq slot (returned to cache)
typedef struct {
    wayT way; // the way to occupy
    Vector#(childNum, DirPend) dirPend; // downgrade children
    Maybe#(Line) data; // data to resp parent
} PRqSlot#(numeric type childNum, type wayT) deriving(Bits, Eq, FShow);

instance DefaultValue#(PRqSlot#(childNum, wayT));
    defaultValue = PRqSlot {
        way: ?,
        dirPend: replicate(Invalid),
        data: Invalid
    };
endinstance

interface PRqMshr#(
    numeric type childNum, 
    numeric type pRqNum, 
    type cRqIndexT, 
    type wayT
);
    method Maybe#(Bit#(TLog#(pRqNum))) getEmptySlot;
    method Action setRq(Bit#(TLog#(pRqNum)) n, PRqMsg#(void) rq);
    method ActionValue#(PRqMsg#(void)) getRq(Bit#(TLog#(pRqNum)) n);
    method ActionValue#(PRqState) getState(Bit#(TLog#(pRqNum)) n);
    method Action setStateSlot(
        Bit#(TLog#(pRqNum)) n, PRqState state, 
        PRqSlot#(childNum, wayT) slot
    );
    method Action setSucc(
        Bit#(TLog#(pRqNum)) n, 
        Maybe#(cRqIndexT) succ // successor must be cRq
    );
    method ActionValue#(Maybe#(cRqIndexT)) getSucc(Bit#(TLog#(pRqNum)) n);
    method ActionValue#(PRqSlot#(childNum, wayT)) getSlot(Bit#(TLog#(pRqNum)) n);
    // find existing pRq which has gone through pipeline, but not in Done state, and has not successor
    // i.e. search the end of dependency chain
    method ActionValue#(Maybe#(Bit#(TLog#(pRqNum)))) searchEndOfChain(Addr addr);
    // find pRq that needs to send req to child to downgrade
    // we can pass in a suggested req idx (which will have priority)
    method Maybe#(Bit#(TLog#(pRqNum))) searchProcessing(Maybe#(Bit#(TLog#(pRqNum))) suggestIdx);
endinterface

module mkPRqMshr(
    PRqMshr#(childNum, pRqNum, cRqIndexT, wayT)
) provisos(
    Alias#(pRqIndexT, Bit#(TLog#(pRqNum))),
    Alias#(slotT, PRqSlot#(childNum, wayT)),
    Alias#(cRqIndexT, Bit#(_cRqIndexSz)),
    Alias#(wayT, Bit#(_waySz))
);
    // MSHR entry state
    Vector#(pRqNum, Reg#(PRqState)) stateVec <- replicateM(mkReg(Empty));
    Vector#(pRqNum, Reg#(PRqMsg#(void))) reqVec <- replicateM(mkRegU);
    // summary bit of dirPend in each entry: asserted when some dirPend[i] = ToSend
    Vector#(pRqNum, Reg#(Bool)) needReqChildVec <- replicateM(mkReg(False));
    // pRq mshr slots
    RegFile#(pRqIndexT, slotT) slotFile <- mkRegFile(0, fromInteger(valueOf(pRqNum) - 1));
    // successor valid bit
    Vector#(pRqNum, Reg#(Bool)) succValidVec <- replicateM(mkReg(False));
    // successor MSHR index (can only be cRq)
    RegFile#(pRqIndexT, cRqIndexT) succFile <- mkRegFile(0, fromInteger(valueOf(pRqNum) - 1));

    method Maybe#(pRqIndexT) getEmptySlot;
        return searchIndex(\== (PRqState'(Empty)), readVector(stateVec));
    endmethod

    method Action setRq(pRqIndexT n, PRqMsg#(void) rq);
        reqVec[n] <= rq;
    endmethod

    method ActionValue#(PRqMsg#(void)) getRq(pRqIndexT n);
        return reqVec[n];
    endmethod

    method ActionValue#(PRqState) getState(pRqIndexT n);
        return stateVec[n];
    endmethod

    method Action setStateSlot(pRqIndexT n, PRqState state, slotT slot);
        stateVec[n] <= state;
        slotFile.upd(n, slot);
        // set dirPend summary bit
        needReqChildVec[n] <= getNeedReqChild(slot.dirPend);
    endmethod

    method Action setSucc(pRqIndexT n, Maybe#(cRqIndexT) succ);
        succValidVec[n] <= isValid(succ);
        succFile.upd(n, fromMaybe(?, succ));
    endmethod

    method ActionValue#(Maybe#(cRqIndexT)) getSucc(pRqIndexT n);
        if(succValidVec[n]) begin
            return Valid (succFile.sub(n));
        end
        else begin
            return Invalid;
        end
    endmethod

    method ActionValue#(slotT) getSlot(pRqIndexT n);
        return slotFile.sub(n);
    endmethod

    method ActionValue#(Maybe#(pRqIndexT)) searchEndOfChain(Addr addr);
        function Bool isEndOfChain(Integer i);
            // check entry i is end of chain or not
            let state = stateVec[i];
            Bool notDone = state != Done;
            Bool processedOnce = state != Empty && state != Init;
            Bool addrMatch = getLineAddr(reqVec[i].addr) == getLineAddr(addr);
            Bool noSucc = !succValidVec[i];
            return notDone && processedOnce && addrMatch && noSucc;
        endfunction
        Vector#(pRqNum, Integer) idxVec = genVector;
        return searchIndex(isEndOfChain, idxVec);
    endmethod

    method Maybe#(pRqIndexT) searchProcessing(Maybe#(pRqIndexT) suggestIdx);
        function Bool isProcessing(pRqIndexT i);
            return stateVec[i] == Processing && needReqChildVec[i];
        endfunction
        if(suggestIdx matches tagged Valid .idx &&& isProcessing(idx)) begin
            return suggestIdx;
        end
        else begin
            Vector#(pRqNum, pRqIndexT) idxVec = genWith(fromInteger);
            return searchIndex(isProcessing, idxVec);
        end
    endmethod
endmodule


