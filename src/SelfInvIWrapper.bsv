
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
import CacheUtils::*;
import Types::*;
import CCTypes::*;
import CCSizes::*;
import SelfInvIPipe::*;
import ICRqMshr::*;
import SelfInvIBank::*;
import RWBramCore::*;
import LruReplace::*;

(* synthesize *)
module mkSelfInvICRqMshrWrapper(
    ICRqMshr#(L1CRqNum, L1Way, ITag, ProcRqToI, L1InstResult)
);
    function Addr getAddrFromReq(ProcRqToI r);
        return r.addr;
    endfunction
    let m <- mkICRqMshr(getAddrFromReq);
    return m;
endmodule

typedef TrueLruRepInfo#(L1WayNum) IRepInfo;

(* synthesize *)
module mkSelfInvIPipeline(
    SelfInvIPipe#(LgIBankNum, L1WayNum, IIndex, ITag, IRepInfo, L1CRqMshrIdx)
);
    RWBramCore#(L1Index, IRepInfo) repRam <- mkRWBramCore;
    ReplacePolicy#(L1WayNum, IRepInfo) repPolicy <- mkTrueLruReplace;
    let m <- mkSelfInvIPipe(repRam, repPolicy);
    return m;
endmodule

typedef SelfInvIBank#(L1ISupSz, LgIBankNum, L1WayNum, IIndexSz, ITagSz, L1CRqNum, IRepInfo) SelfInvIBankWrapper;

(* synthesize *)
module mkSelfInvIBankWrapper(SelfInvIBankWrapper);
    let m <- mkSelfInvIBank(0, mkSelfInvICRqMshrWrapper, mkSelfInvIPipeline);
    return m;
endmodule
