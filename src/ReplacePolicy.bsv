
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
import CCTypes::*;

// general replacement method: given a suggested way according to replacement
// policy, produce the way to be replaced. The way to be replaced must not be
// locked by any pending req.
function Maybe#(wayT) getReplaceWay(
    Vector#(wayNum, Bool) unlocked,
    Vector#(wayNum, Bool) invalid,
    wayT suggestedWay
) provisos (
    Alias#(wayT, Bit#(TLog#(wayNum)))
);
    // first search for invalid & unlocked way
    function Bool isInvUnlock(Integer i);
        return unlocked[i] && invalid[i];
    endfunction
    Vector#(wayNum, Integer) idxVec = genVector;
    Maybe#(wayT) repWay = searchIndex(isInvUnlock, idxVec);
    if(!isValid(repWay)) begin
        // check whether the suggested way (by replacement policy) is unlocked
        if(unlocked[suggestedWay]) begin
            repWay = Valid (suggestedWay);
        end
        else begin
            // just find a unlocked way
            repWay = searchIndex(id, unlocked);
        end
    end
    return repWay;
endfunction

// random replacement
typedef Bit#(0) RandRepInfo;

function RandRepInfo updateRandRep(RandRepInfo orig, wayT hitWay);
    return 0;
endfunction

interface RandomWay#(numeric type wayNum);
    method Bit#(TLog#(wayNum)) randomWay;
endinterface

module mkRandomWay(RandomWay#(wayNum)) provisos(
    Alias#(wayT, Bit#(TLog#(wayNum)))
);
    Reg#(wayT) randWay <- mkReg(0);

    rule tick;
        randWay <= randWay == fromInteger(valueOf(wayNum) - 1) ? 0 : randWay + 1;
    endrule
    
    method wayT randomWay;
        return randWay;
    endmethod
endmodule

// true LRU replacement
// index 0 -- MRU, index wayNum-1 -- LRU
typedef Vector#(wayNum, Bit#(TLog#(wayNum))) LRURepInfo#(numeric type wayNum);

function LRURepInfo#(wayNum) getLRUInitVal;
    return genWith(fromInteger);
endfunction

function Bit#(TLog#(wayNum)) getLRUWay(LRURepInfo#(wayNum) rep);
    return rep[valueof(wayNum) - 1];
endfunction

function LRURepInfo#(wayNum) updateLRURep(LRURepInfo#(wayNum) repInfo, Bit#(TLog#(wayNum)) way);
    LRURepInfo#(wayNum) newInfo = repInfo;
    newInfo[0] = way; // MRU
    Bool findWay = False;
    for(Integer i = 1; i < valueof(wayNum); i = i+1) begin
        findWay = findWay || (repInfo[i-1] == way);
        if(!findWay) begin
            newInfo[i] = repInfo[i-1];
        end
    end
    return newInfo;
endfunction

// tree LRU replacement (require wayNum to be power of 2)
// {TreeLRURepInfo, 1'b0} encodes a tree
// level 0 (root): Bit idx 'b1
// level 1: Bit idx 'b10 ~ 'b11
// level 2: Bit idx 'b100 ~ 'b111
// Total log(wayNum) levels
// For Bit idx [x], value = 0 points to left node Bit idx [x * 2], value = 1
// points to right node Bit idx [x * 2 + 1]
typedef Bit#(TSub#(wayNum, 1)) TreeLRURepInfo#(numeric type wayNum);

function TreeLRURepInfo#(wayNum) getTreeLRUInitVal;
    return 0;
endfunction

function Bit#(TLog#(wayNum)) getTreeLRUWay(TreeLRURepInfo#(wayNum) rep) provisos(
    Add#(TExp#(TLog#(wayNum)), 0, wayNum),
    Add#(TLog#(wayNum), a__, TAdd#(TLog#(wayNum), 1))
);
    // traverse the tree to a leaf node
    Bit#(wayNum) tree = {rep, 1'b0};
    Bit#(TLog#(wayNum)) idx = 1;
    for(Integer i = 0; i < valueof(TLog#(wayNum)); i = i+1) begin
        idx = truncate({idx, tree[idx]});
    end
    return idx;
endfunction

function TreeLRURepInfo#(wayNum) updateTreeLRURep(TreeLRURepInfo#(wayNum) orig, Bit#(TLog#(wayNum)) way) provisos(
    Add#(TExp#(TLog#(wayNum)), 0, wayNum),
    Add#(1, TSub#(wayNum, 1), wayNum),
    Add#(TLog#(wayNum), a__, TAdd#(TLog#(wayNum), 1))
);
    Bit#(wayNum) tree = {orig, 1'b0};
    Bit#(TLog#(wayNum)) idx = 1;
    for(Integer i = valueof(TLog#(wayNum)) - 1; i >= 0; i = i-1) begin
        tree[idx] = ~(way[i]); // way is MRU, should point to oppsite direction
        idx = truncate({idx, way[i]});
    end
    return truncateLSB(tree);
endfunction
