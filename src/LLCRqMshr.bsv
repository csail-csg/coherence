import Vector::*;
import GetPut::*;
import RegFile::*;
import FIFO::*;
import FShow::*;
import Types::*;
import CCTypes::*;
import DefaultValue::*;
import Ehr::*;
import MshrDeadlockChecker::*;

// MSHR dependency chain invariant:
// every cRq and pRq (for same addr) which has gone through pipeline once will be linked into the chain

// in LLC, the head (h1) of a chain may be linked as the successor of the head (h2) of another chain
// when h2 is replacing the addr of h1
// h1 should be waken up and sent to pipeline when replacement is done (i.e. h2 gets to WaitSt)

// CRq MSHR entry state
typedef enum {
    Empty,
    Init,
    WaitOldTag,
    WaitSt,
    Done,
    Depend
} LLCRqState deriving(Bits, Eq, FShow);

// we split data from slot info
// because data may be used to buffer mem resp data
typedef struct {
    wayT way; // the way to occupy
    tagT repTag; // tag being replaced, used in sending down req to children
    Bool waitP; // wait parent resp
    Vector#(childNum, DirPend) dirPend; // pending child downgrade
} LLCRqSlot#(numeric type childNum, type wayT, type tagT) deriving(Bits, Eq, FShow);

instance DefaultValue#(LLCRqSlot#(childNum, wayT, tagT));
    defaultValue = LLCRqSlot {
        way: ?,
        repTag: ?,
        waitP: False,
        dirPend: replicate(Invalid)
    };
endinstance

typedef struct {
    reqT req;
    LLCRqState state;
    Bool waitP;
    Vector#(childNum, DirPend) dirPend;
} LLCRqMshrStuck#(numeric type childNum, type reqT) deriving(Bits, Eq, FShow);

// interface for cRq/mRs/cRsTransfer
interface LLCRqMshr_transfer#(
    numeric type childNum,
    numeric type cRqNum,
    type wayT,
    type tagT,
    type reqT // child req type
);
    method ActionValue#(reqT) getRq(Bit#(TLog#(cRqNum)) n);
    method ActionValue#(LLCRqSlot#(childNum, wayT, tagT)) getSlot(Bit#(TLog#(cRqNum)) n);
    method ActionValue#(Bit#(TLog#(cRqNum))) getEmptyEntryInit(reqT r, Maybe#(Line) d);
endinterface

// interface for mRsDeq
interface LLCRqMshr_mRsDeq#(numeric type cRqNum);
    method Action setData(Bit#(TLog#(cRqNum)) n, Maybe#(Line) d);
endinterface

// interface for sendToM
interface LLCRqMshr_sendToM#(
    numeric type childNum,
    numeric type cRqNum,
    type wayT,
    type tagT,
    type reqT // child req type
);
    method ActionValue#(reqT) getRq(Bit#(TLog#(cRqNum)) n);
    method ActionValue#(LLCRqSlot#(childNum, wayT, tagT)) getSlot(Bit#(TLog#(cRqNum)) n);
    method ActionValue#(Maybe#(Line)) getData(Bit#(TLog#(cRqNum)) n);
endinterface

// interface for sendRsToDma and sendRsToC
interface LLCRqMshr_sendRsToDmaC#(
    numeric type cRqNum,
    type reqT // child req type
);
    method ActionValue#(reqT) getRq(Bit#(TLog#(cRqNum)) n);
    method ActionValue#(Maybe#(Line)) getData(Bit#(TLog#(cRqNum)) n);
    method Action releaseEntry(Bit#(TLog#(cRqNum)) n);
endinterface

// interface for sendRqToC
interface LLCRqMshr_sendRqToC#(
    numeric type childNum,
    numeric type cRqNum,
    type wayT,
    type tagT,
    type reqT // child req type
);
    method ActionValue#(reqT) getRq(Bit#(TLog#(cRqNum)) n);
    method ActionValue#(LLCRqState) getState(Bit#(TLog#(cRqNum)) n);
    method ActionValue#(LLCRqSlot#(childNum, wayT, tagT)) getSlot(Bit#(TLog#(cRqNum)) n);
    method Action setSlot(Bit#(TLog#(cRqNum)) n, LLCRqSlot#(childNum, wayT, tagT) s);
    // find cRq that needs to send req to child to downgrade
    // (either replacement, or incompatible children states)
    // we can pass in a suggested req idx (which will have priority)
    method Maybe#(Bit#(TLog#(cRqNum))) searchNeedRqChild(Maybe#(Bit#(TLog#(cRqNum))) suggestIdx);
endinterface

// interface for pipelineResp_xxx
interface LLCRqMshr_pipelineResp#(
    numeric type childNum,
    numeric type cRqNum,
    type wayT,
    type tagT,
    type reqT // child req type
);
    method ActionValue#(reqT) getRq(Bit#(TLog#(cRqNum)) n);
    method ActionValue#(LLCRqState) getState(Bit#(TLog#(cRqNum)) n);
    method ActionValue#(LLCRqSlot#(childNum, wayT, tagT)) getSlot(Bit#(TLog#(cRqNum)) n);
    method ActionValue#(Maybe#(Line)) getData(Bit#(TLog#(cRqNum)) n);
    method ActionValue#(Maybe#(Bit#(TLog#(cRqNum)))) getAddrSucc(Bit#(TLog#(cRqNum)) n);
    method ActionValue#(Maybe#(Bit#(TLog#(cRqNum)))) getRepSucc(Bit#(TLog#(cRqNum)) n);
    method Action setData(Bit#(TLog#(cRqNum)) n, Maybe#(Line) d);
    method Action setStateSlot(
        Bit#(TLog#(cRqNum)) n, LLCRqState state, 
        LLCRqSlot#(childNum, wayT, tagT) slot
    );
    method Action setAddrSucc( // same address successor
        Bit#(TLog#(cRqNum)) n, 
        Maybe#(Bit#(TLog#(cRqNum))) succ
    );
    method Action setRepSucc( // successor due to replacement
        Bit#(TLog#(cRqNum)) n, 
        Maybe#(Bit#(TLog#(cRqNum))) succ
    );
    // find existing cRq which has gone through pipeline, but not in Done state, and has no addr successor
    // (it could have rep successor)
    // i.e. search the end of dependency chain for req to the same addr
    method ActionValue#(Maybe#(Bit#(TLog#(cRqNum)))) searchEndOfChain(Addr addr);
endinterface

interface LLCRqMshr#(
    numeric type childNum, 
    numeric type cRqNum, 
    type wayT,
    type tagT,
    type reqT // child req type
);
    interface LLCRqMshr_transfer#(childNum, cRqNum, wayT, tagT, reqT) transfer;
    interface LLCRqMshr_mRsDeq#(cRqNum) mRsDeq;
    interface LLCRqMshr_sendToM#(childNum, cRqNum, wayT, tagT, reqT) sendToM;
    interface LLCRqMshr_sendRsToDmaC#(cRqNum, reqT) sendRsToDmaC;
    interface LLCRqMshr_sendRqToC#(childNum, cRqNum, wayT, tagT, reqT) sendRqToC;
    interface LLCRqMshr_pipelineResp#(childNum, cRqNum, wayT, tagT, reqT) pipelineResp;
    // detect deadlock: only in use when macro CHECK_DEADLOCK is defined
    interface Get#(LLCRqMshrStuck#(childNum, reqT)) stuck;
endinterface

//////////////////
// safe version //
//////////////////
module mkLLCRqMshrSafe#(
    function Addr getAddrFromReq(reqT r)
)(
    LLCRqMshr#(childNum, cRqNum, wayT, tagT, reqT)
) provisos (
    Alias#(cRqIndexT, Bit#(TLog#(cRqNum))),
    Alias#(slotT, LLCRqSlot#(childNum, wayT, tagT)),
    Alias#(wayT, Bit#(_waySz)),
    Alias#(tagT, Bit#(_tagSz)),
    Bits#(reqT, _reqSz)
);
    // logical ordering: sendToM < transfer < sendRqToC < sendRsToDma/C < mRsDeq
    // EHR ports
    Integer sendToM_port = 0; // sendToM is read-only, so use port 0
    Integer transfer_port = 0; // cRqTransfer_xx, mRsTransfer_send
    Integer sendRqToC_port = 1; // read req/state/slot, write slot
    Integer sendRsToDmaC_port = 1; // sendRsToDma/C doesn't read slot
    Integer mRsDeq_port = 1; // mRsDeq only writes data
    Integer pipelineResp_port = 2;

    // cRq req contents
    Vector#(cRqNum, Ehr#(3, reqT)) reqVec <- replicateM(mkEhr(?));
    // MSHR entry state
    Vector#(cRqNum, Ehr#(3, LLCRqState)) stateVec <- replicateM(mkEhr(Empty));
    // summary bit of dirPend in each entry: asserted when some dirPend[i] = ToSend
    Vector#(cRqNum, Ehr#(3, Bool)) needReqChildVec <- replicateM(mkEhr(False));
    // cRq mshr slots
    Vector#(cRqNum, Ehr#(3, slotT)) slotVec <- replicateM(mkEhr(defaultValue));
    // data valid bit
    Vector#(cRqNum, Ehr#(3, Bool)) dataValidVec <- replicateM(mkEhr(False));
    // data values
    Vector#(cRqNum, Ehr#(3, Line)) dataVec <- replicateM(mkEhr(?));
    // successor valid bit
    Vector#(cRqNum, Ehr#(3, Bool)) addrSuccValidVec <- replicateM(mkEhr(False));
    Vector#(cRqNum, Ehr#(3, Bool)) repSuccValidVec <- replicateM(mkEhr(False));
    // successor MSHR index
    RegFile#(cRqIndexT, cRqIndexT) addrSuccFile <- mkRegFile(0, fromInteger(valueOf(cRqNum) - 1));
    RegFile#(cRqIndexT, cRqIndexT) repSuccFile <- mkRegFile(0, fromInteger(valueOf(cRqNum) - 1));
    // empty entry FIFO
    FIFO#(cRqIndexT) emptyEntryQ <- mkSizedFIFO(valueOf(cRqNum));

    // empty entry FIFO needs initialization
    Reg#(Bool) inited <- mkReg(False);
    Reg#(cRqIndexT) initIdx <- mkReg(0);

    rule initEmptyEntry(!inited);
        emptyEntryQ.enq(initIdx);
        initIdx <= initIdx + 1;
        if(initIdx == fromInteger(valueOf(cRqNum) - 1)) begin
            inited <= True;
            $display("%t LLCRqMshrSafe %m: init empty entry done", $time);
        end
    endrule

`ifdef CHECK_DEADLOCK
    MshrDeadlockChecker#(cRqNum) checker <- mkMshrDeadlockChecker;
    FIFO#(LLCRqMshrStuck#(childNum, reqT)) stuckQ <- mkFIFO1;

    (* fire_when_enabled *)
    rule checkDeadlock;
        let stuckIdx <- checker.getStuckIdx;
        if(stuckIdx matches tagged Valid .n) begin
            stuckQ.enq(LLCRqMshrStuck {
                req: reqVec[n][0],
                state: stateVec[n][0],
                waitP: slotVec[n][0].waitP,
                dirPend: slotVec[n][0].dirPend
            });
        end
    endrule
`endif

    function Action writeSlot(Integer ehrPort, cRqIndexT n, slotT s);
    action
        slotVec[n][ehrPort] <= s;
        // set dirPend summary bit
        needReqChildVec[n][ehrPort] <= getNeedReqChild(s.dirPend);
    endaction
    endfunction

    interface LLCRqMshr_transfer transfer;
        method ActionValue#(reqT) getRq(cRqIndexT n);
            return reqVec[n][transfer_port];
        endmethod

        method ActionValue#(slotT) getSlot(cRqIndexT n);
            return slotVec[n][transfer_port];
        endmethod

        method ActionValue#(cRqIndexT) getEmptyEntryInit(reqT r, Maybe#(Line) d) if(inited);
            emptyEntryQ.deq;
            cRqIndexT n = emptyEntryQ.first;
            reqVec[n][transfer_port] <= r;
            stateVec[n][transfer_port] <= Init;
            writeSlot(transfer_port, n, defaultValue);
            dataValidVec[n][transfer_port] <= isValid(d);
            dataVec[n][transfer_port] <= validValue(d);
            addrSuccValidVec[n][transfer_port] <= False;
            repSuccValidVec[n][transfer_port] <= False;
`ifdef CHECK_DEADLOCK
            checker.initEntry(n);
`endif
            return n;
        endmethod
    endinterface

    interface LLCRqMshr_mRsDeq mRsDeq;
        method Action setData(cRqIndexT n, Maybe#(Line) d);
            dataValidVec[n][mRsDeq_port] <= isValid(d);
            dataVec[n][mRsDeq_port] <= fromMaybe(?, d);
        endmethod
    endinterface

    interface LLCRqMshr_sendToM sendToM;
        method ActionValue#(reqT) getRq(cRqIndexT n);
            return reqVec[n][sendToM_port];
        endmethod

        method ActionValue#(slotT) getSlot(cRqIndexT n);
            return slotVec[n][sendToM_port];
        endmethod

        method ActionValue#(Maybe#(Line)) getData(cRqIndexT n);
            return dataValidVec[n][sendToM_port] ? Valid (dataVec[n][sendToM_port]) : Invalid;
        endmethod
    endinterface

    interface LLCRqMshr_sendRsToDmaC sendRsToDmaC;
        method ActionValue#(reqT) getRq(cRqIndexT n);
            return reqVec[n][sendRsToDmaC_port];
        endmethod

        method ActionValue#(Maybe#(Line)) getData(cRqIndexT n);
            return dataValidVec[n][sendRsToDmaC_port] ? Valid (dataVec[n][sendRsToDmaC_port]) : Invalid;
        endmethod

        method Action releaseEntry(cRqIndexT n) if(inited);
            emptyEntryQ.enq(n);
            stateVec[n][sendRsToDmaC_port] <= Empty;
`ifdef CHECK_DEADLOCK
            checker.releaseEntry(n);
`endif
        endmethod
    endinterface

    interface LLCRqMshr_sendRqToC sendRqToC;
        method ActionValue#(reqT) getRq(cRqIndexT n);
            return reqVec[n][sendRqToC_port];
        endmethod

        method ActionValue#(LLCRqState) getState(cRqIndexT n);
            return stateVec[n][sendRqToC_port];
        endmethod

        method ActionValue#(slotT) getSlot(cRqIndexT n);
            return slotVec[n][sendRqToC_port];
        endmethod

        method Action setSlot(cRqIndexT n, slotT s);
            writeSlot(sendRqToC_port, n, s);
        endmethod

        method Maybe#(cRqIndexT) searchNeedRqChild(Maybe#(cRqIndexT) suggestIdx);
            function Bool isNeedRqChild(cRqIndexT i);
                return (stateVec[i][sendRqToC_port] == WaitOldTag || stateVec[i][sendRqToC_port] == WaitSt)
                    && needReqChildVec[i][sendRqToC_port];
            endfunction
            if(suggestIdx matches tagged Valid .idx &&& isNeedRqChild(idx)) begin
                return suggestIdx;
            end
            else begin
                Vector#(cRqNum, cRqIndexT) idxVec = genWith(fromInteger);
                return searchIndex(isNeedRqChild, idxVec);
            end
        endmethod
    endinterface

    interface LLCRqMshr_pipelineResp pipelineResp;
        method ActionValue#(reqT) getRq(cRqIndexT n);
            return reqVec[n][pipelineResp_port];
        endmethod

        method ActionValue#(LLCRqState) getState(cRqIndexT n);
            return stateVec[n][pipelineResp_port];
        endmethod

        method ActionValue#(slotT) getSlot(cRqIndexT n);
            return slotVec[n][pipelineResp_port];
        endmethod

        method ActionValue#(Maybe#(Line)) getData(cRqIndexT n);
            return dataValidVec[n][pipelineResp_port] ? Valid (dataVec[n][pipelineResp_port]) : Invalid;
        endmethod

        method ActionValue#(Maybe#(cRqIndexT)) getAddrSucc(cRqIndexT n);
            return addrSuccValidVec[n][pipelineResp_port] ? Valid (addrSuccFile.sub(n)) : Invalid;
        endmethod

        method ActionValue#(Maybe#(cRqIndexT)) getRepSucc(cRqIndexT n);
            return repSuccValidVec[n][pipelineResp_port] ? Valid (repSuccFile.sub(n)) : Invalid;
        endmethod

        method Action setData(cRqIndexT n, Maybe#(Line) d);
            dataValidVec[n][pipelineResp_port] <= isValid(d);
            dataVec[n][pipelineResp_port] <= fromMaybe(?, d);
        endmethod

        method Action setStateSlot(cRqIndexT n, LLCRqState state, slotT slot);
            stateVec[n][pipelineResp_port] <= state;
            writeSlot(pipelineResp_port, n, slot);
        endmethod

        method Action setAddrSucc(cRqIndexT n, Maybe#(cRqIndexT) succ);
            addrSuccValidVec[n][pipelineResp_port] <= isValid(succ);
            addrSuccFile.upd(n, fromMaybe(?, succ));
        endmethod

        method Action setRepSucc(cRqIndexT n, Maybe#(cRqIndexT) succ);
            repSuccValidVec[n][pipelineResp_port] <= isValid(succ);
            repSuccFile.upd(n, fromMaybe(?, succ));
        endmethod

        method ActionValue#(Maybe#(cRqIndexT)) searchEndOfChain(Addr addr);
            function Bool isEndOfChain(Integer i);
                // check entry i is end of chain or not
                let state = stateVec[i][pipelineResp_port];
                Bool notDone = state != Done;
                Bool processedOnce = state != Empty && state != Init;
                Bool addrMatch = getLineAddr(getAddrFromReq(reqVec[i][pipelineResp_port])) == getLineAddr(addr);
                Bool noAddrSucc = !addrSuccValidVec[i][pipelineResp_port];
                return notDone && processedOnce && addrMatch && noAddrSucc;
            endfunction
            Vector#(cRqNum, Integer) idxVec = genVector;
            return searchIndex(isEndOfChain, idxVec);
        endmethod
    endinterface

`ifdef CHECK_DEADLOCK
    interface stuck = toGet(stuckQ);
`else
    interface stuck = nullGet;
`endif
endmodule


////////////////////
// unsafe version //
////////////////////
module mkLLCRqMshrUnsafe#(
    function Addr getAddrFromReq(reqT r)
)(
    LLCRqMshr#(childNum, cRqNum, wayT, tagT, reqT)
) provisos (
    Alias#(cRqIndexT, Bit#(TLog#(cRqNum))),
    Alias#(slotT, LLCRqSlot#(childNum, wayT, tagT)),
    Alias#(wayT, Bit#(_waySz)),
    Alias#(tagT, Bit#(_tagSz)),
    Bits#(reqT, _reqSz)
);
    // EHR ports
    Integer read_port = 0;
    Integer cRqTransfer_new_port = 0;
    Integer mRsDeq_port = 1;
    Integer sendRsToDmaC_port = 1;
    Integer sendRqToC_port = 1;
    Integer pipelineResp_port = 2;

    // cRq req contents
    Vector#(cRqNum, Reg#(reqT)) reqVec <- replicateM(mkRegU);
    // MSHR entry state
    Vector#(cRqNum, Ehr#(3, LLCRqState)) stateVec <- replicateM(mkEhr(Empty));
    // cRq mshr slots
    Vector#(cRqNum, Ehr#(3, slotT)) slotVec <- replicateM(mkEhr(defaultValue));
    // summary bit of dirPend in each entry: asserted when some dirPend[i] = ToSend, set together with slotVec
    Vector#(cRqNum, Ehr#(3, Bool)) needReqChildVec <- replicateM(mkEhr(False));
    // data
    Vector#(cRqNum, Ehr#(3, Maybe#(Line))) dataVec <- replicateM(mkEhr(Invalid));
    // successor valid bit
    Vector#(cRqNum, Ehr#(3, Bool)) addrSuccValidVec <- replicateM(mkEhr(False));
    Vector#(cRqNum, Ehr#(3, Bool)) repSuccValidVec <- replicateM(mkEhr(False));
    // successor MSHR index
    RegFile#(cRqIndexT, cRqIndexT) addrSuccFile <- mkRegFile(0, fromInteger(valueOf(cRqNum) - 1));
    RegFile#(cRqIndexT, cRqIndexT) repSuccFile <- mkRegFile(0, fromInteger(valueOf(cRqNum) - 1));
    // empty entry FIFO
    FIFO#(cRqIndexT) emptyEntryQ <- mkSizedFIFO(valueOf(cRqNum));

    // empty entry FIFO needs initialization
    Reg#(Bool) inited <- mkReg(False);
    Reg#(cRqIndexT) initIdx <- mkReg(0);

    rule initEmptyEntry(!inited);
        emptyEntryQ.enq(initIdx);
        initIdx <= initIdx + 1;
        if(initIdx == fromInteger(valueOf(cRqNum) - 1)) begin
            inited <= True;
            $display("%t LLCRqMshrUnsafe %m: init empty entry done", $time);
        end
    endrule

`ifdef CHECK_DEADLOCK
    MshrDeadlockChecker#(cRqNum) checker <- mkMshrDeadlockChecker;
    FIFO#(LLCRqMshrStuck#(childNum, reqT)) stuckQ <- mkFIFO1;

    (* fire_when_enabled *)
    rule checkDeadlock;
        let stuckIdx <- checker.getStuckIdx;
        if(stuckIdx matches tagged Valid .n) begin
            stuckQ.enq(LLCRqMshrStuck {
                req: reqVec[n],
                state: stateVec[n][0],
                waitP: slotVec[n][0].waitP,
                dirPend: slotVec[n][0].dirPend
            });
        end
    endrule
`endif

    // wires for write methods
    RWire#(Tuple3#(cRqIndexT, reqT, Maybe#(Line))) cRqTransfer_new_getEmptyEntryInit <- mkRWire; // new cRq
    RWire#(Tuple2#(cRqIndexT, Maybe#(Line))) mRsDeq_setData <- mkRWire; // cRq get mem resp
    RWire#(cRqIndexT) sendRsToDmaC_releaseEntry <- mkRWire; // cRq to resp child or DMA
    RWire#(Tuple2#(cRqIndexT, slotT)) sendRqToC_setSlot <- mkRWire; // cRq to downgrade child
    RWire#(Tuple2#(cRqIndexT, Maybe#(Line))) pipelineResp_setData <- mkRWire; // hit cRq
    RWire#(Tuple3#(cRqIndexT, LLCRqState, slotT)) pipelineResp_setStateSlot <- mkRWire; // all cRq
    RWire#(Tuple2#(cRqIndexT, Maybe#(cRqIndexT))) pipelineResp_setAddrSucc <- mkRWire; // cRq being depended on
    RWire#(Tuple2#(cRqIndexT, Maybe#(cRqIndexT))) pipelineResp_setRepSucc <- mkRWire; // cRq being depended on

    // read wires
    RWire#(cRqIndexT) transfer_read <- mkRWire; // cRq with mem resp, cRq retry
    //RWire#(cRqIndexT) sendToM_read <- mkRWire; // sendToM only reads, no need to check
    RWire#(cRqIndexT) pipelineResp_read <- mkRWire; // should = pipelineResp_setStateSlot
    // sendRsToDmaC, sendRqToC: read index = write index

    function Action writeSlot(Integer ehrPort, cRqIndexT n, slotT s);
    action
        slotVec[n][ehrPort] <= s;
        // set dirPend summary bit
        needReqChildVec[n][ehrPort] <= getNeedReqChild(s.dirPend);
    endaction
    endfunction

    (* fire_when_enabled, no_implicit_conditions *)
    rule cRqTransfer_new_con;
        if(cRqTransfer_new_getEmptyEntryInit.wget matches tagged Valid {.n, .r, .d}) begin
            reqVec[n] <= r;
            stateVec[n][cRqTransfer_new_port] <= Init;
            writeSlot(cRqTransfer_new_port, n, defaultValue);
            dataVec[n][cRqTransfer_new_port] <= d;
            addrSuccValidVec[n][cRqTransfer_new_port] <= False;
            repSuccValidVec[n][cRqTransfer_new_port] <= False;
        end
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule mRsDeq_sendToC_con;
        if(mRsDeq_setData.wget matches tagged Valid {.n, .d}) begin
            dataVec[n][mRsDeq_port] <= d;
        end
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule sendRsToDmaC_con;
        if(sendRsToDmaC_releaseEntry.wget matches tagged Valid .n) begin
            stateVec[n][sendRsToDmaC_port] <= Empty;
        end
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule sendRqToC_con;
        if(sendRqToC_setSlot.wget matches tagged Valid {.n, .s}) begin
            writeSlot(sendRqToC_port, n, s);
        end
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule pipelineResp_con;
        if(pipelineResp_setData.wget matches tagged Valid {.n, .d}) begin
            dataVec[n][pipelineResp_port] <= d;
        end
        if(pipelineResp_setStateSlot.wget matches tagged Valid {.n, .state, .slot}) begin
            stateVec[n][pipelineResp_port] <= state;
            writeSlot(pipelineResp_port, n, slot);
        end
        if(pipelineResp_setAddrSucc.wget matches tagged Valid {.n, .succ}) begin
            addrSuccValidVec[n][pipelineResp_port] <= isValid(succ);
            addrSuccFile.upd(n, fromMaybe(?, succ));
        end
        if(pipelineResp_setRepSucc.wget matches tagged Valid {.n, .succ}) begin
            repSuccValidVec[n][pipelineResp_port] <= isValid(succ);
            repSuccFile.upd(n, fromMaybe(?, succ));
        end
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule checkConflict;
        // pipelineResp read should = write
        if(pipelineResp_setStateSlot.wget matches tagged Valid {.n, .st, .sl}) begin
            doAssert(pipelineResp_read.wget == Valid (n), "pipeline valid read = valid write");
        end
        else begin
            doAssert(pipelineResp_read.wget == Invalid, "pipelineResp invalid read = invalid write");
        end

        // pipelineResp if setData, then setStateSlot should be same
        if(pipelineResp_setData.wget matches tagged Valid {.n, .d}) begin
            if(pipelineResp_setStateSlot.wget matches tagged Valid {.m, .st, .sl}) begin
                doAssert(n == m, "pipelineResp setData valid then should = setStateSlot");
            end
            else begin
                doAssert(False, "pipelineResp setData valid then setStateSlot valid");
            end
        end

        // rules in LLBank should all access different cRq index
        if(transfer_read.wget matches tagged Valid .n) begin
            if(cRqTransfer_new_getEmptyEntryInit.wget matches tagged Valid {.m, .r}) begin
                doAssert(n != m, "transfer_read conflcits with cRqTransfer_new_getEmptyEntryInit");
            end
            if(mRsDeq_setData.wget matches tagged Valid {.m, .d}) begin
                doAssert(n != m, "transfer_read conflicts with mRsDeq_setData");
            end
            if(sendRsToDmaC_releaseEntry.wget matches tagged Valid .m) begin
                doAssert(n != m, "transfer_read conflicts with sendRsToDmaC_releaseEntry");
            end
            if(sendRqToC_setSlot.wget matches tagged Valid {.m, .s}) begin
                doAssert(n != m, "transfer_read conflicts with sendRqToC_setSlot");
            end
            if(pipelineResp_setStateSlot.wget matches tagged Valid {.m, .st, .sl}) begin
                doAssert(n != m, "transfer_read conflicts with pipelineResp_setStateSlot");
            end
        end
        if(cRqTransfer_new_getEmptyEntryInit.wget matches tagged Valid {.n, .r}) begin
            if(mRsDeq_setData.wget matches tagged Valid {.m, .d}) begin
                doAssert(n != m, "cRqTransfer_new_getEmptyEntryInit conflicts with mRsDeq_setData");
            end
            if(sendRsToDmaC_releaseEntry.wget matches tagged Valid .m) begin
                doAssert(n != m, "cRqTransfer_new_getEmptyEntryInit conflicts with sendRsToDmaC_releaseEntry");
            end
            if(sendRqToC_setSlot.wget matches tagged Valid {.m, .s}) begin
                doAssert(n != m, "cRqTransfer_new_getEmptyEntryInit conflicts with sendRqToC_setSlot");
            end
            if(pipelineResp_setStateSlot.wget matches tagged Valid {.m, .st, .sl}) begin
                doAssert(n != m, "cRqTransfer_new_getEmptyEntryInit conflicts with pipelineResp_setStateSlot");
            end
            if(pipelineResp_setAddrSucc.wget matches tagged Valid {.m, .s}) begin
                doAssert(n != m, "cRqTransfer_new_getEmptyEntryInit conflicts with pipelineResp_setAddrSucc");
            end
            if(pipelineResp_setRepSucc.wget matches tagged Valid {.m, .s}) begin
                doAssert(n != m, "cRqTransfer_new_getEmptyEntryInit conflicts with pipelineResp_setRepSucc");
            end
        end
        if(mRsDeq_setData.wget matches tagged Valid {.n, .d}) begin
            if(sendRsToDmaC_releaseEntry.wget matches tagged Valid .m) begin
                doAssert(n != m, "mRsDeq_setData conflicts with sendRsToDmaC_releaseEntry");
            end
            if(sendRqToC_setSlot.wget matches tagged Valid {.m, .s}) begin
                doAssert(n != m, "mRsDeq_setData conflicts with sendRqToC_setSlot");
            end
            if(pipelineResp_setStateSlot.wget matches tagged Valid {.m, .st, .sl}) begin
                doAssert(n != m, "mRsDeq_setData conflicts with pipelineResp_setStateSlot");
            end
        end
        if(sendRsToDmaC_releaseEntry.wget matches tagged Valid .n) begin
            if(sendRqToC_setSlot.wget matches tagged Valid {.m, .s}) begin
                doAssert(n != m, "sendRsToDmaC_releaseEntry conflicts with sendRqToC_setSlot");
            end
            if(pipelineResp_setStateSlot.wget matches tagged Valid {.m, .st, .sl}) begin
                doAssert(n != m, "sendRsToDmaC_releaseEntry conflicts with pipelineResp_setStateSlot");
            end
        end
        if(sendRqToC_setSlot.wget matches tagged Valid {.n, .s}) begin
            if(pipelineResp_setStateSlot.wget matches tagged Valid {.m, .st, .sl}) begin
                doAssert(n != m, "sendRqToC_setSlot conflicts with pipelineResp_setStateSlot");
            end
        end
    endrule

    interface LLCRqMshr_transfer transfer;
        method ActionValue#(reqT) getRq(cRqIndexT n);
            transfer_read.wset(n);
            return reqVec[n];
        endmethod

        method ActionValue#(slotT) getSlot(cRqIndexT n);
            return slotVec[n][read_port];
        endmethod

        method ActionValue#(cRqIndexT) getEmptyEntryInit(reqT r, Maybe#(Line) d) if(inited);
            emptyEntryQ.deq;
            cRqIndexT n = emptyEntryQ.first;
            cRqTransfer_new_getEmptyEntryInit.wset(tuple3(n, r, d));
`ifdef CHECK_DEADLOCK
            checker.initEntry(n);
`endif
            return n;
        endmethod
    endinterface

    interface LLCRqMshr_mRsDeq mRsDeq;
        method Action setData(cRqIndexT n, Maybe#(Line) d);
            mRsDeq_setData.wset(tuple2(n, d));
        endmethod
    endinterface

    interface LLCRqMshr_sendToM sendToM;
        method ActionValue#(reqT) getRq(cRqIndexT n);
            //sendToM_read.wset(n);
            return reqVec[n];
        endmethod

        method ActionValue#(slotT) getSlot(cRqIndexT n);
            return slotVec[n][read_port];
        endmethod

        method ActionValue#(Maybe#(Line)) getData(cRqIndexT n);
            return dataVec[n][read_port];
        endmethod
    endinterface

    interface LLCRqMshr_sendRsToDmaC sendRsToDmaC;
        method ActionValue#(reqT) getRq(cRqIndexT n);
            return reqVec[n];
        endmethod

        method ActionValue#(Maybe#(Line)) getData(cRqIndexT n);
            return dataVec[n][read_port];
        endmethod

        method Action releaseEntry(cRqIndexT n) if(inited);
            emptyEntryQ.enq(n);
            sendRsToDmaC_releaseEntry.wset(n);
`ifdef CHECK_DEADLOCK
            checker.releaseEntry(n);
`endif
        endmethod
    endinterface

    interface LLCRqMshr_sendRqToC sendRqToC;
        method ActionValue#(reqT) getRq(cRqIndexT n);
            return reqVec[n];
        endmethod

        method ActionValue#(LLCRqState) getState(cRqIndexT n);
            return stateVec[n][read_port];
        endmethod

        method ActionValue#(slotT) getSlot(cRqIndexT n);
            return slotVec[n][read_port];
        endmethod

        method Action setSlot(cRqIndexT n, slotT s);
            sendRqToC_setSlot.wset(tuple2(n, s));
        endmethod

        method Maybe#(cRqIndexT) searchNeedRqChild(Maybe#(cRqIndexT) suggestIdx);
            function Bool isNeedRqChild(cRqIndexT i);
                return (stateVec[i][read_port] == WaitOldTag || stateVec[i][read_port] == WaitSt)
                    && needReqChildVec[i][read_port];
            endfunction
            if(suggestIdx matches tagged Valid .idx &&& isNeedRqChild(idx)) begin
                return suggestIdx;
            end
            else begin
                Vector#(cRqNum, cRqIndexT) idxVec = genWith(fromInteger);
                return searchIndex(isNeedRqChild, idxVec);
            end
        endmethod
    endinterface

    interface LLCRqMshr_pipelineResp pipelineResp;
        method ActionValue#(reqT) getRq(cRqIndexT n);
            pipelineResp_read.wset(n);
            return reqVec[n];
        endmethod

        method ActionValue#(LLCRqState) getState(cRqIndexT n);
            return stateVec[n][read_port];
        endmethod

        method ActionValue#(slotT) getSlot(cRqIndexT n);
            return slotVec[n][read_port];
        endmethod

        method ActionValue#(Maybe#(Line)) getData(cRqIndexT n);
            return dataVec[n][read_port];
        endmethod

        method ActionValue#(Maybe#(cRqIndexT)) getAddrSucc(cRqIndexT n);
            return addrSuccValidVec[n][read_port] ? Valid (addrSuccFile.sub(n)) : Invalid;
        endmethod

        method ActionValue#(Maybe#(cRqIndexT)) getRepSucc(cRqIndexT n);
            return repSuccValidVec[n][read_port] ? Valid (repSuccFile.sub(n)) : Invalid;
        endmethod

        method Action setData(cRqIndexT n, Maybe#(Line) d);
            pipelineResp_setData.wset(tuple2(n, d));
        endmethod

        method Action setStateSlot(cRqIndexT n, LLCRqState state, slotT slot);
            pipelineResp_setStateSlot.wset(tuple3(n, state, slot));
        endmethod

        method Action setAddrSucc(cRqIndexT n, Maybe#(cRqIndexT) succ);
            pipelineResp_setAddrSucc.wset(tuple2(n, succ));
        endmethod

        method Action setRepSucc(cRqIndexT n, Maybe#(cRqIndexT) succ);
            pipelineResp_setRepSucc.wset(tuple2(n, succ));
        endmethod

        method ActionValue#(Maybe#(cRqIndexT)) searchEndOfChain(Addr addr);
            function Bool isEndOfChain(Integer i);
                // check entry i is end of chain or not
                let state = stateVec[i][read_port];
                Bool notDone = state != Done;
                Bool processedOnce = state != Empty && state != Init;
                Bool addrMatch = getLineAddr(getAddrFromReq(reqVec[i])) == getLineAddr(addr);
                Bool noAddrSucc = !addrSuccValidVec[i][read_port];
                return notDone && processedOnce && addrMatch && noAddrSucc;
            endfunction
            Vector#(cRqNum, Integer) idxVec = genVector;
            return searchIndex(isEndOfChain, idxVec);
        endmethod
    endinterface

`ifdef CHECK_DEADLOCK
    interface stuck = toGet(stuckQ);
`else
    interface stuck = nullGet;
`endif
endmodule


// exported version
module mkLLCRqMshr#(
    function Addr getAddrFromReq(reqT r)
)(
    LLCRqMshr#(childNum, cRqNum, wayT, tagT, reqT)
) provisos (
    Alias#(wayT, Bit#(_waySz)),
    Alias#(tagT, Bit#(_tagSz)),
    Bits#(reqT, _reqSz)
);
`ifdef UNSAFE_LL_CRQ_MSHR
    let m <- mkLLCRqMshrUnsafe(getAddrFromReq);
`else
    let m <- mkLLCRqMshrSafe(getAddrFromReq);
`endif
    return m;
endmodule
