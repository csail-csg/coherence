import FIFO::*;
import GetPut::*;
import ClientServer::*;
import Connectable::*;
import Vector::*;
import RegFile::*;
import Types::*;
import CCTypes::*;
import DelayMemTypes::*;
import Fifo::*;
import CacheUtils::*;

typedef struct {
    Addr addr;
    Bool wr;
    Line data;
} IdealDelayMemReq deriving(Bits, Eq);

// delay: number of pipeline stages to delay the response
// logMemNumBytes: log size of memory (in term of bytes)
interface IdealDelayMem#(
    numeric type delay, 
    numeric type logMemNumBytes,
    type rqIdT,
    type childT
);
    interface DelayMemTest to_test;
    interface MemFifoServer#(rqIdT, childT) to_proc;
endinterface

module mkIdealDelayMem(IdealDelayMem#(delay, lgMemSzBytes, rqIdT, childT)) provisos(
    NumAlias#(memAddrWidth, TSub#(lgMemSzBytes, LgLineSzBytes)),
    Alias#(memAddr, Bit#(memAddrWidth)),
    Alias#(toMemT, ToMemMsg#(rqIdT, childT)),
    Alias#(memRsT, MemRsMsg#(rqIdT, childT)),
    Add#(memAddrWidth, a__, AddrSz),
    Bits#(rqIdT, _rqIdSz),
    Bits#(childT, _childSz)
);
    function memAddr getMemAddr(Addr a);
        return truncate(a >> valueOf(LgLineSzBytes));
    endfunction

    function Action checkAddrOverflow(Addr a);
        return action
            if(a >= fromInteger(valueOf(TExp#(lgMemSzBytes)))) begin
                $fwrite(stderr, "[IdealDelayMem] ERROR: time %t, addr %x overflow\n", $time, a);
                $finish;
            end
        endaction;
    endfunction

    Reg#(Bool) inited <- mkReg(False);
    RegFile#(memAddr, Line) mem <- mkRegFileFull;
    Vector#(delay, FIFO#(memRsT)) rdRespQ <- replicateM(mkFIFO);
    Fifo#(2, toMemT) inQ <- mkCFFifo;
    Fifo#(2, memRsT) outQ <- mkCFFifo;

    for(Integer i = 0; i < valueOf(delay) - 1; i = i+1) begin
        mkConnection(toGet(rdRespQ[i]), toPut(rdRespQ[i+1]));
    end
    mkConnection(toGet(rdRespQ[valueOf(delay) - 1]), toPut(outQ));

    rule doLd(inited &&& inQ.first matches tagged Ld .ld);
        inQ.deq;
        checkAddrOverflow(ld.addr);
        let mAddr = getMemAddr(ld.addr);
        rdRespQ[0].enq(MemRsMsg {
            data: mem.sub(mAddr),
            child: ld.child,
            id: ld.id
        });
    endrule

    rule doWb(inited &&& inQ.first matches tagged Wb .wb);
        inQ.deq;
        checkAddrOverflow(wb.addr);
        let mAddr = getMemAddr(wb.addr);
        // calculate write line
        Vector#(LineSzBytes, Bit#(8)) curLine = unpack(pack(mem.sub(mAddr)));
        Vector#(LineSzBytes, Bit#(8)) wrLine = unpack(pack(wb.data));
        function Bit#(8) getNewByte(Integer i);
            return wb.byteEn[i] ? wrLine[i] : curLine[i];
        endfunction
        Vector#(LineSzBytes, Integer) idxVec = genVector;
        Line newLine = unpack(pack(map(getNewByte, idxVec)));
        // update mem
        mem.upd(mAddr, newLine);
    endrule

    interface DelayMemTest to_test;
        method Action initLine(Addr a, Line d) if(!inited);
            checkAddrOverflow(a);
            let mAddr = getMemAddr(a);
            mem.upd(mAddr, d);
        endmethod

        method Action initDone if(!inited);
            inited <= True;
        endmethod

        method Line getLine(Addr a);
            return mem.sub(getMemAddr(a));
        endmethod
    endinterface

    interface MemFifoServer to_proc;
        interface fromC = toFifoEnq(inQ);
        interface rsToC = toFifoDeq(outQ);
    endinterface
endmodule

