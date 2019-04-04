
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
import SelfInvL1Pipe::*;
import L1CRqMshr::*;
import L1PRqMshr::*;
import SelfInvL1Bank::*;
import RWBramCore::*;
import LruReplace::*;

(* synthesize *)
module mkSelfInvL1CRqMshrWrapper(
    L1CRqMshr#(L1CRqNum, L1Way, L1Tag, ProcRq#(ProcRqId))
);
    function Addr getAddrFromReq(ProcRq#(ProcRqId) r);
        return r.addr;
    endfunction
    function Bool isReqToExclusive(ProcRq#(ProcRqId) r);
        return r.toState >= E;
    endfunction
    let m <- mkL1CRqMshr(getAddrFromReq, isReqToExclusive);
    return m;
endmodule

(* synthesize *)
module mkSelfInvL1PRqMshrWrapper(
    L1PRqMshr#(L1PRqNum)
);
    let m <- mkL1PRqMshr;
    return m;
endmodule

typedef TrueLruRepInfo#(L1WayNum) L1RepInfo;

(* synthesize *)
module mkSelfInvL1Pipeline(
    SelfInvL1Pipe#(LgL1BankNum, L1WayNum, L1DMaxHitNum, L1Index, L1Tag, L1RepInfo, L1CRqMshrIdx, L1PRqMshrIdx)
);
    RWBramCore#(L1Index, L1RepInfo) repRam <- mkRWBramCore;
    ReplacePolicy#(L1WayNum, L1RepInfo) repPolicy <- mkTrueLruReplace;
    let m <- mkSelfInvL1Pipe(repRam, repPolicy);
    return m;
endmodule

typedef SelfInvL1Bank#(LgL1BankNum, L1WayNum, L1IndexSz, L1TagSz, L1CRqNum, L1PRqNum, L1DMaxHitNum, L1RepInfo, ProcRqId) SelfInvL1CacheWrapper;

module mkSelfInvL1CacheWrapper#(L1ProcResp#(ProcRqId) procResp)(SelfInvL1CacheWrapper);
    let m <- mkSelfInvL1Cache(mkSelfInvL1CRqMshrWrapper, mkSelfInvL1PRqMshrWrapper, mkSelfInvL1Pipeline, procResp);
    return m;
endmodule
