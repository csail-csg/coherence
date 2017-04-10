import Types::*;
import CacheUtils::*;
import MemoryTypes::*;
import CCTypes::*;
import FShow::*;
import Fifo::*;
import MsgFifo::*;

interface MemBank#(
    numeric type lgBankNum,
    numeric type ldQSz, // load Q size
    numeric type wrBSz, // write buffer size
    type childT,
    type ldRqIdT
);
    interface MemFifoServer#(ldRqIdT, childT) to_proc;
    interface RiscyAxiMaster to_mem;
endinterface

typedef struct {
    childT child;
    ldRqIdT id;
} LdMemTag#(type ldRqIdT, type childT) deriving(Bits, Eq, FShow);

module mkMemBank(
    MemBank#(lgBankNum, ldQSz, wrBSz, childT, ldRqIdT)
) provisos (
    Alias#(ldTagT, LdMemTag#(ldRqIdT, childT)),
    Alias#(toMemT, ToMemMsg#(ldRqIdT, childT)),
    Alias#(memRsT, MemRsMsg#(ldRqIdT, childT)),
    Add#(1, a__, wrBSz),
    Alias#(childT, Bit#(_childSz)),
    Alias#(ldRqIdT, Bit#(_ldRqIdT))
);

    Fifo#(2, toMemT) fromCQ <- mkCFFifo;
    Fifo#(2, memRsT) rsToCQ <- mkCFFifo;
    CacheToAxi#(wrBSz, ldQSz, ldTagT) axiMem <- mkCacheToAxi;

    rule sendToMem;
        fromCQ.deq;
        $display("%t Mem %m sendToMem: ", $time, fshow(fromCQ.first));
        case(fromCQ.first) matches
            tagged Ld .rq: begin
                axiMem.msgToAxi.enqReq(CacheReqToMem {
                    cAddr: getLineAddr(rq.addr),
                    id: LdMemTag {child: rq.child, id: rq.id}
                });
            end
            tagged Wb .rs: begin
                axiMem.msgToAxi.enqResp(CacheRespToMem {
                    cAddr: getLineAddr(rs.addr),
                    data: pack(rs.data)
                });
            end
            default: begin
                doAssert(False, "impossible toMem type");
            end
        endcase
    endrule

    rule deqWbDone;
        axiMem.wbDone.deq;
    endrule

    rule getMemResp;
        axiMem.ldResp.deq;
        MemRespToCache#(ldTagT) resp = axiMem.ldResp.first;
        memRsT mRs = MemRsMsg {
            data: unpack(resp.data),
            child: resp.id.child,
            id: resp.id.id
        };
        rsToCQ.enq(mRs);
        $display("%t Mem %m getMemResp: ", $time, fshow(mRs));
    endrule

    interface MemFifoServer to_proc;
        interface fromCPut = toFifoEnq(fromCQ);
        interface rsToCGet = toFifoDeq(rsToCQ);
    endinterface
    interface to_mem = axiMem.to_mem;
endmodule

