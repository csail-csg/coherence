import Vector::*;
import CacheUtils::*;
import Types::*;
import CCTypes::*;
import CCSizes::*;
import L1Pipe::*;
import ICRqMshr::*;
import IPRqMshr::*;
import IBank::*;

(* synthesize *)
module mkICRqMshrWrapper(
    ICRqMshr#(L1CRqNum, L1Way, ITag, ProcRqToI, L1InstResult)
);
    function Addr getAddrFromReq(ProcRqToI r);
        return r.addr;
    endfunction
    let m <- mkICRqMshr(getAddrFromReq);
    return m;
endmodule

(* synthesize *)
module mkIPRqMshrWrapper(
    IPRqMshr#(L1PRqNum)
);
    let m <- mkIPRqMshr;
    return m;
endmodule

(* synthesize *)
module mkIPipeline(
    L1Pipe#(LgIBankNum, L1WayNum, IIndex, ITag, L1CRqMshrIdx, L1PRqMshrIdx)
);
    let m <- mkL1Pipe;
    return m;
endmodule

typedef IBank#(L1ISupSz, LgIBankNum, L1WayNum, IIndexSz, ITagSz, L1CRqNum, L1PRqNum) IBankWrapper;

(* synthesize *)
module mkIBankWrapper(IBankWrapper);
    let m <- mkIBank(mkICRqMshrWrapper, mkIPRqMshrWrapper, mkIPipeline);
    return m;
endmodule
