
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

import CacheUtils::*;
import Types::*;
import CCTypes::*;
import CCSizes::*;
import IntPipe::*;
import CRqMshr::*;
import PRqMshr::*;
import IntBank::*;

(* synthesize *)
module mkL2CRqMshr(
    CRqMshr#(L2ChildNum, L2CRqNum, L2PRqMshrIdx, L2Way, L2Tag, CRqMsg#(L2CRqId, L2Child))
);
    function Addr getAddrFromReq(CRqMsg#(L2CRqId, L2Child) r);
        return r.addr;
    endfunction
    let m <- mkCRqMshr(getAddrFromReq);
    return m;
endmodule

(* synthesize *)
module mkL2PRqMshr(
    PRqMshr#(L2ChildNum, L2PRqNum, L2CRqMshrIdx, L2Way)
);
    let m <- mkPRqMshr;
    return m;
endmodule

(* synthesize *)
module mkL2Pipeline(
    IntPipe#(LgL2BankNum, L2ChildNum, L2WayNum, L2Index, L2Tag, L2CRqMshrIdx, L2PRqMshrIdx)
);
    let m <- mkIntPipe;
    return m;
endmodule

typedef IntBank#(LgL2BankNum, L2ChildNum, L2WayNum, L2IndexSz, L2TagSz, L2CRqNum, L2PRqNum, L2CRqId) L2BankWrapper;

(* synthesize *)
module mkL2BankWrapper(L2BankWrapper);
    let m <- mkIntBank(mkL2CRqMshr, mkL2PRqMshr, mkL2Pipeline);
    return m;
endmodule
