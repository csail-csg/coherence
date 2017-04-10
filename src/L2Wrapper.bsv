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
