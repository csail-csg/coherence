import DirBank::*;
import Types::*;
import CCTypes::*;
import CacheUtils::*;
import CCSizes::*;
import CRqMshr::*;

(* synthesize *)
module mkDirCRqMshr(
    CRqMshr#(DirChildNum, DirCRqNum, Bit#(0), Bit#(0), Bit#(0), CRqMsg#(DirCRqId, DirChild))
);
    function Addr getAddr(CRqMsg#(DirCRqId, DirChild) r) = r.addr;
    let m <- mkCRqMshr(getAddr);
    return m;
endmodule


typedef DirBank#(LgDirBankNum, DirChildNum, DirIndexSz, DirCRqNum, DirCRqId) DirBankWrapper;

(* synthesize *)
module mkDirBankWrapper(DirBankWrapper);
    let m <- mkDirBank(mkDirCRqMshr);
    return m;
endmodule
