import CacheUtils::*;
import Types::*;
import CCTypes::*;
import CCSizes::*;
import L1Pipe::*;
import L1CRqMshr::*;
import L1PRqMshr::*;
import L1Bank::*;

(* synthesize *)
module mkL1CRqMshrWrapper(
    L1CRqMshr#(L1CRqNum, L1Way, L1Tag, ProcRq#(ProcRqId))
);
    function Addr getAddrFromReq(ProcRq#(ProcRqId) r);
        return r.addr;
    endfunction
    let m <- mkL1CRqMshr(getAddrFromReq);
    return m;
endmodule

(* synthesize *)
module mkL1PRqMshrWrapper(
    L1PRqMshr#(L1PRqNum)
);
    let m <- mkL1PRqMshr;
    return m;
endmodule

(* synthesize *)
module mkL1Pipeline(
    L1Pipe#(LgL1BankNum, L1WayNum, L1Index, L1Tag, L1CRqMshrIdx, L1PRqMshrIdx)
);
    let m <- mkL1Pipe;
    return m;
endmodule

typedef L1Bank#(LgL1BankNum, L1WayNum, L1IndexSz, L1TagSz, L1CRqNum, L1PRqNum, ProcRqId) L1CacheWrapper;

module mkL1CacheWrapper#(L1ProcResp#(ProcRqId) procResp)(L1CacheWrapper);
    let m <- mkL1Cache(mkL1CRqMshrWrapper, mkL1PRqMshrWrapper, mkL1Pipeline, procResp);
    return m;
endmodule
