import Vector::*;
import GetPut::*;
import RegFile::*;
import FIFO::*;
import FShow::*;
import GetPut::*;
import Types::*;
import CCTypes::*;
import DefaultValue::*;
import Ehr::*;
import MshrDeadlockChecker::*;

// MSHR dependency chain invariant:
// every cRq and pRq (for same addr) which has gone through pipeline once will be linked into the chain

// in L1, pRq is always directly handled at the end of pipeline

// PRq MSHR entry state
typedef enum {
    Empty,
    Init,
    Done
} IPRqState deriving (Bits, Eq, FShow);

typedef struct {
    Addr addr;
    Msi toState;
    IPRqState state;
} IPRqMshrStuck deriving(Bits, Eq, FShow);

interface IPRqMshr_sendRsToP_pRq#(numeric type pRqNum);
    method ActionValue#(PRqMsg#(void)) getRq(Bit#(TLog#(pRqNum)) n);
    method Action releaseEntry(Bit#(TLog#(pRqNum)) n);
endinterface

interface IPRqMshr_pipelineResp#(numeric type pRqNum);
    method ActionValue#(PRqMsg#(void)) getRq(Bit#(TLog#(pRqNum)) n);
    method Action releaseEntry(Bit#(TLog#(pRqNum)) n);
    method Action setDone(Bit#(TLog#(pRqNum)) n);
endinterface

interface IPRqMshr#(numeric type pRqNum);
    // port for pRqTransfer
    method ActionValue#(Bit#(TLog#(pRqNum))) getEmptyEntryInit(PRqMsg#(void) r);

    // port for sendRsToP_pRq
    interface IPRqMshr_sendRsToP_pRq#(pRqNum) sendRsToP_pRq;

    // port for pipelineResp
    interface IPRqMshr_pipelineResp#(pRqNum) pipelineResp;

    // detect deadlock: only in use when macro CHECK_DEADLOCK is defined
    interface Get#(IPRqMshrStuck) stuck;
endinterface


//////////////////
// safe version //
//////////////////
module mkIPRqMshrSafe(
    IPRqMshr#(pRqNum)
) provisos(
    Alias#(pRqIndexT, Bit#(TLog#(pRqNum)))
);
    // EHR port
    Integer pRqTransfer_port = 2;
    Integer sendRsToP_pRq_port = 1;
    Integer pipelineResp_port = 0;

    // MSHR entry state
    Vector#(pRqNum, Ehr#(3, IPRqState)) stateVec <- replicateM(mkEhr(Empty));
    Vector#(pRqNum, Ehr#(3, PRqMsg#(void))) reqVec <- replicateM(mkEhr(?));
    // empty entry FIFO
    FIFO#(pRqIndexT) emptyEntryQ <- mkSizedFIFO(valueOf(pRqNum));

    // empty entry FIFO needs initialization
    Reg#(Bool) inited <- mkReg(False);
    Reg#(pRqIndexT) initIdx <- mkReg(0);

    // released entry index fifos
    FIFO#(pRqIndexT) releaseEntryQ_sendRsToP_pRq <- mkFIFO;
    FIFO#(pRqIndexT) releaseEntryQ_pipelineResp <- mkFIFO;

    rule initEmptyEntry(!inited);
        emptyEntryQ.enq(initIdx);
        initIdx <= initIdx + 1;
        if(initIdx == fromInteger(valueOf(pRqNum) - 1)) begin
            inited <= True;
            $display("%t IPRqMshrSafe %m: init empty entry done", $time);
        end
    endrule

`ifdef CHECK_DEADLOCK
    MshrDeadlockChecker#(pRqNum) checker <- mkMshrDeadlockChecker;
    FIFO#(IPRqMshrStuck) stuckQ <- mkFIFO1;

    (* fire_when_enabled *)
    rule checkDeadlock;
        let stuckIdx <- checker.getStuckIdx;
        if(stuckIdx matches tagged Valid .n) begin
            stuckQ.enq(IPRqMshrStuck {
                addr: reqVec[n][0].addr,
                toState: reqVec[n][0].toState,
                state: stateVec[n][0]
            });
        end
    endrule
`endif

    rule doReleaseEntry_sendRsToP_pRq(inited);
        let n <- toGet(releaseEntryQ_sendRsToP_pRq).get;
        emptyEntryQ.enq(n);
`ifdef CHECK_DEADLOCK
        checker.releaseEntry(n);
`endif
    endrule

    (* descending_urgency = "doReleaseEntry_sendRsToP_pRq, doReleaseEntry_pipelineResp" *)
    rule doReleaseEntry_pipelineResp(inited);
        let n <- toGet(releaseEntryQ_pipelineResp).get;
        emptyEntryQ.enq(n);
`ifdef CHECK_DEADLOCK
        checker.releaseEntry(n);
`endif
    endrule

    method ActionValue#(pRqIndexT) getEmptyEntryInit(PRqMsg#(void) r) if(inited);
        emptyEntryQ.deq;
        pRqIndexT n = emptyEntryQ.first;
        stateVec[n][pRqTransfer_port] <= Init;
        reqVec[n][pRqTransfer_port] <= r;
`ifdef CHECK_DEADLOCK
        checker.initEntry(n);
`endif
        return n;
    endmethod

    interface IPRqMshr_sendRsToP_pRq sendRsToP_pRq;
        method ActionValue#(PRqMsg#(void)) getRq(pRqIndexT n);
            return reqVec[n][sendRsToP_pRq_port];
        endmethod

        method Action releaseEntry(pRqIndexT n) if(inited);
            releaseEntryQ_sendRsToP_pRq.enq(n);
            stateVec[n][sendRsToP_pRq_port] <= Empty;
        endmethod
    endinterface

    interface IPRqMshr_pipelineResp pipelineResp;
        method ActionValue#(PRqMsg#(void)) getRq(pRqIndexT n);
            return reqVec[n][pipelineResp_port];
        endmethod

        method Action setDone(pRqIndexT n);
            stateVec[n][pipelineResp_port] <= Done;
        endmethod

        method Action releaseEntry(pRqIndexT n) if(inited);
            releaseEntryQ_pipelineResp.enq(n);
            stateVec[n][pipelineResp_port] <= Empty;
        endmethod
    endinterface

`ifdef CHECK_DEADLOCK
    interface stuck = toGet(stuckQ);
`else
    interface stuck = nullGet;
`endif
endmodule


/*
////////////////////
// unsafe version //
////////////////////
module mkIPRqMshrUnsafe(
    IPRqMshr#(pRqNum)
) provisos(
    Alias#(pRqIndexT, Bit#(TLog#(pRqNum)))
);
    // EHR read port
    Integer read_port = 0;
    // EHR write port
    Integer pRqTransfer_port = 0;
    Integer sendRsToP_pRq_port = 1;
    Integer pipelineResp_port = 2;

    // MSHR entry state
    Vector#(pRqNum, Ehr#(3, IPRqState)) stateVec <- replicateM(mkEhr(Empty));
    Vector#(pRqNum, Reg#(PRqMsg#(void))) reqVec <- replicateM(mkRegU);
    // data valid bits
    Vector#(pRqNum, Ehr#(3, Bool)) dataValidVec <- replicateM(mkEhr(False));
    // data values
    RegFile#(pRqIndexT, Line) dataFile <- mkRegFile(0, fromInteger(valueOf(pRqNum) - 1));
    // empty entry FIFO
    FIFO#(pRqIndexT) emptyEntryQ <- mkSizedFIFO(valueOf(pRqNum));

    // empty entry FIFO needs initialization
    Reg#(Bool) inited <- mkReg(False);
    Reg#(pRqIndexT) initIdx <- mkReg(0);

    // released entry index fifos
    FIFO#(pRqIndexT) releaseEntryQ_sendRsToP_pRq <- mkFIFO;
    FIFO#(pRqIndexT) releaseEntryQ_pipelineResp <- mkFIFO;

    // Wires for write methods
    RWire#(Tuple2#(pRqIndexT, PRqMsg#(void))) pRqTransfer_getEmptyEntryInit <- mkRWire;
    
    RWire#(pRqIndexT) sendRsToP_pRq_releaseEntry <- mkRWire;

    RWire#(pRqIndexT) pipelineResp_releaseEntry <- mkRWire;
    RWire#(Tuple2#(pRqIndexT, Maybe#(Line))) pipelineResp_setDone_setData <- mkRWire;

    // Wires for read methods: for checking
    RWire#(pRqIndexT) sendRsToP_pRq_read <- mkRWire;
    RWire#(pRqIndexT) pipelineResp_read <- mkRWire;

    rule initEmptyEntry(!inited);
        emptyEntryQ.enq(initIdx);
        initIdx <= initIdx + 1;
        if(initIdx == fromInteger(valueOf(pRqNum) - 1)) begin
            inited <= True;
            $display("%t IPRqMshrUnsafe %m: init empty entry done", $time);
        end
    endrule

    rule doReleaseEntry_sendRsToP_pRq(inited);
        let n <- toGet(releaseEntryQ_sendRsToP_pRq).get;
        emptyEntryQ.enq(n);
    endrule

    (* descending_urgency = "doReleaseEntry_sendRsToP_pRq, doReleaseEntry_pipelineResp" *)
    rule doReleaseEntry_pipelineResp(inited);
        let n <- toGet(releaseEntryQ_pipelineResp).get;
        emptyEntryQ.enq(n);
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule con_pRqTransfer;
        if(pRqTransfer_getEmptyEntryInit.wget matches tagged Valid {.n, .r}) begin
            stateVec[n][pRqTransfer_port] <= Init;
            dataValidVec[n][pRqTransfer_port] <= False;
            reqVec[n] <= r;
        end
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule con_sendRsToP_pRq;
        if(sendRsToP_pRq_releaseEntry.wget matches tagged Valid .n) begin
            stateVec[n][sendRsToP_pRq_port] <= Empty;
        end
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule con_pipelineResp;
        doAssert(!(isValid(pipelineResp_releaseEntry.wget) && isValid(pipelineResp_setDone_setData.wget)),
            "pipelineResp cannot simultaneously release entry and set done data"
        );
        if(pipelineResp_releaseEntry.wget matches tagged Valid .n) begin
            stateVec[n][pipelineResp_port] <= Empty;
        end
        else if(pipelineResp_setDone_setData.wget matches tagged Valid {.n, .line}) begin
            stateVec[n][pipelineResp_port] <= Done;
            dataValidVec[n][pipelineResp_port] <= isValid(line);
            dataFile.upd(n, fromMaybe(?, line));
        end
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule checkInvariant;
        // get write index
        Maybe#(pRqIndexT) pRqTransfer_write = Invalid;
        if(pRqTransfer_getEmptyEntryInit.wget matches tagged Valid {.n, .r}) begin
            pRqTransfer_write = Valid (n);
        end
        
        Maybe#(pRqIndexT) sendRsToP_pRq_write = sendRsToP_pRq_releaseEntry.wget;

        Maybe#(pRqIndexT) pipelineResp_write = Invalid;
        if(pipelineResp_releaseEntry.wget matches tagged Valid .n) begin
            pipelineResp_write = Valid (n);
        end
        else if(pipelineResp_setDone_setData.wget matches tagged Valid {.n, .line}) begin
            pipelineResp_write = Valid (n);
        end

        // read write should be in pair
        doAssert(sendRsToP_pRq_read.wget == sendRsToP_pRq_write,
            "sendRsToP_pRq read write index should match"
        );
        doAssert(pipelineResp_read.wget == pipelineResp_write,
            "pipelineResp read write index should match"
        );

        // check rw conflicts
        if(pRqTransfer_write matches tagged Valid .n) begin
            doAssert(sendRsToP_pRq_write != Valid (n),
                "pRqTransfer write conflicts with sendRsToP_pRq write"
            );
            doAssert(sendRsToP_pRq_read.wget != Valid (n),
                "pRqTransfer write conflicts with sendRsToP_pRq read"
            );
            doAssert(pipelineResp_write != Valid (n),
                "pRqTransfer write conflicts with pipelineResp write"
            );
            doAssert(pipelineResp_read.wget != Valid (n),
                "pRqTransfer write conflicts with pipelineResp read"
            );
        end
        if(sendRsToP_pRq_write matches tagged Valid .n) begin
            doAssert(pipelineResp_write != Valid (n),
                "sendRsToP_pRq write conflicts with pipelineResp write"
            );
            doAssert(pipelineResp_read.wget != Valid (n),
                "sendRsToP_pRq write conflicts with pipelineResp read"
            );
        end
        if(pipelineResp_write matches tagged Valid .n) begin
            doAssert(sendRsToP_pRq_write != Valid (n),
                "pipelineResp write conflicts with sendRsToP_pRq write"
            );
            doAssert(sendRsToP_pRq_read.wget != Valid (n),
                "pipelineResp write conflicts with sendRsToP_pRq read"
            );
        end
    endrule

    method ActionValue#(pRqIndexT) getEmptyEntryInit(PRqMsg#(void) r) if(inited);
        emptyEntryQ.deq;
        pRqIndexT n = emptyEntryQ.first;
        pRqTransfer_getEmptyEntryInit.wset(tuple2(n, r));
        return n;
    endmethod

    interface IPRqMshr_sendRsToP_pRq sendRsToP_pRq;
        method ActionValue#(PRqMsg#(void)) getRq(pRqIndexT n);
            sendRsToP_pRq_read.wset(n); // record read index
            return reqVec[n];
        endmethod

        method ActionValue#(Maybe#(Line)) getData(pRqIndexT n);
            return dataValidVec[n][read_port] ? (Valid (dataFile.sub(n))) : Invalid;
        endmethod

        method Action releaseEntry(pRqIndexT n) if(inited);
            releaseEntryQ_sendRsToP_pRq.enq(n);
            sendRsToP_pRq_releaseEntry.wset(n);
        endmethod
    endinterface

    interface IPRqMshr_pipelineResp pipelineResp;
        method ActionValue#(PRqMsg#(void)) getRq(pRqIndexT n);
            pipelineResp_read.wset(n); // record read index
            return reqVec[n];
        endmethod

        method ActionValue#(IPRqState) getState(pRqIndexT n);
            return stateVec[n][read_port];
        endmethod

        method Action setDone_setData(pRqIndexT n, Maybe#(Line) line);
            pipelineResp_setDone_setData.wset(tuple2(n, line));
        endmethod

        method Action releaseEntry(pRqIndexT n) if(inited);
            releaseEntryQ_pipelineResp.enq(n);
            pipelineResp_releaseEntry.wset(n);
        endmethod
    endinterface
endmodule
*/

// exportd version
module mkIPRqMshr(IPRqMshr#(pRqNum));
`ifdef UNSAFE_I_PRQ_MSHR
    let m <- mkIPRqMshrUnsafe;
`else
    let m <- mkIPRqMshrSafe;
`endif
    return m;
endmodule
