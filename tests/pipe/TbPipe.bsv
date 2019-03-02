
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
import CCPipe::*;
import RWBramCore::*;
import Ehr::*;
import Randomizable::*;
import FShow::*;
import RegFile::*;
import CCTypes::*;

typedef 10000 TestNum;
typedef Bit#(TLog#(TestNum)) TestId;
typedef Bit#(TLog#(TAdd#(TestNum, 1))) TestCnt;

typedef 4 WayNum;
typedef 2 IndexNum;

typedef Bit#(16) Tag;
typedef Bit#(2) MsiT;
typedef Bit#(8) Dir;
typedef Bit#(4) Owner;
typedef Bit#(8) Other;
typedef Bit#(8) RepInfo;
typedef Bit#(64) Line;
typedef Bit#(32) Data;

typedef Bit#(TLog#(WayNum)) Way;
typedef Bit#(TLog#(IndexNum)) Index;

typedef struct {
    TestId id;
    Index index;
    Way way;
    Bool pRqMiss;
    // random data (to test swapping in new pipe cmd)
    Data data; 
} PipeCmd deriving(Bits, Eq, FShow);

typedef CacheInfo#(Tag, MsiT, Dir, Owner, Other) InfoType;
typedef RamData#(Tag, MsiT, Dir, Owner, Other, Line) RamDataType;
typedef RespState#(MsiT) RespStateType;
typedef PipeOut#(Way, Tag, MsiT, Dir, Owner, Other, RepInfo, Line, PipeCmd) PipeOutType;

typedef struct {
    PipeCmd cmd;
    Maybe#(Line) respLine;
    RespStateType toState;
} EnqInfo deriving(Bits, Eq, FShow);

typedef struct {
    PipeCmd cmd;
    Vector#(WayNum, Tag) tagVec;
    Vector#(WayNum, MsiT) csVec;
    Vector#(WayNum, Owner) ownerVec;
    RepInfo repInfo;
} TagMatchInfo deriving(Bits, Eq, FShow);

typedef struct {
    RepInfo oldInfo;
    Way way;
    RepInfo newInfo;
} UpdateRepInfo deriving(Bits, Eq, FShow);

typedef struct {
    Maybe#(PipeCmd) newCmd;
    RamDataType wrRam;
    Bool updateRep;
} DeqInfo deriving(Bits, Eq, FShow);

typedef Bit#(2) RandEnq;
function Bool getEnq(RandEnq r);
    return r > 0;
endfunction

typedef Bit#(2) RandLineValid;
function Maybe#(Line) getRespLine(RandLineValid r, Line l);
    return r > 2 ? Valid (l) : Invalid;
endfunction

typedef Bit#(2) RandToStateValid;
function RespStateType getToState(RandToStateValid r, MsiT s);
    return (case(r)
        0: UpCs (s);
        1: DownDir (s);
        default: Invalid;
    endcase);
endfunction

typedef Bit#(2) RandSwapDeq;
typedef enum {None, Swap, Deq} SwapDeq deriving(Bits, Eq);
function SwapDeq getSwapDeq(RandSwapDeq r);
    return (case(r)
        0: Swap;
        1, 2: Deq;
        default: None;
    endcase);
endfunction

// in-flight pipeIn, TestId modulo 4 (pipeline only 3 stages)
typedef 2 LogMaxPending;
typedef Bit#(LogMaxPending) PendIdx;
typedef TExp#(LogMaxPending) MaxPending;
function PendIdx getPendIdx(TestCnt n);
    return truncate(n);
endfunction

typedef enum {Init, Process, Check} TestState deriving(Bits, Eq, FShow);

(* synthesize *)
module mkTbPipe(Empty);
    // reference
    Vector#(IndexNum, Vector#(WayNum, Reg#(Line))) refDataRam <- replicateM(replicateM(mkReg(0)));
    Vector#(IndexNum, Vector#(WayNum, Reg#(InfoType))) refInfoRam <- replicateM(replicateM(mkReg(unpack(0))));
    Vector#(IndexNum, Reg#(RepInfo)) refRepInfoRam <- replicateM(mkReg(0));

    // Wires to record info
    RWire#(TagMatchInfo) tmInfo <- mkRWire;
    RWire#(UpdateRepInfo) updateRepInfo <- mkRWire;
    RWire#(PipeOutType) pipeOut <- mkRWire;
    RWire#(EnqInfo) enqInfo <- mkRWire;
    RWire#(DeqInfo) deqInfo <- mkRWire;
    // record in-flight pipeIn
    Reg#(Vector#(MaxPending, EnqInfo)) pendReq <- mkReg(replicate(?));
    // send & recv test cnts
    Reg#(TestCnt) sendCnt <- mkReg(0);
    Reg#(TestCnt) recvCnt <- mkReg(0);
    // randomizers for pipeline input
    Randomize#(RandEnq) randEnq <- mkGenericRandomizer;
    Randomize#(Index) randInIndex <- mkGenericRandomizer;
    Randomize#(Way) randInWay <- mkGenericRandomizer;
    Randomize#(Bool) randInMiss <- mkGenericRandomizer;
    Randomize#(Data) randInData <- mkGenericRandomizer;
    Randomize#(RandLineValid) randLineValid <- mkGenericRandomizer;
    Randomize#(Line) randRespLine <- mkGenericRandomizer;
    Randomize#(RandToStateValid) randToStateValid <- mkGenericRandomizer;
    Randomize#(MsiT) randRespState <- mkGenericRandomizer;
    // randomizers for pipeline output
    Randomize#(RandSwapDeq) randSwapDeq <- mkGenericRandomizer;
    Randomize#(Data) randSwapData <- mkGenericRandomizer;
    Randomize#(Tag) randWrTag <- mkGenericRandomizer;
    Randomize#(MsiT) randWrCs <- mkGenericRandomizer;
    Randomize#(Dir) randWrDir <- mkGenericRandomizer;
    Randomize#(Owner) randWrOwner <- mkGenericRandomizer;
    Randomize#(Other) randWrOther <- mkGenericRandomizer;
    Randomize#(Bool) randWrUpdateRep <- mkGenericRandomizer;
    Randomize#(Line) randWrLine <- mkGenericRandomizer;
    // check ptrs
    Reg#(Index) checkIndex <- mkReg(0);
    Reg#(Bool) waitRamOut <- mkReg(False);
    // init ptr
    Reg#(Index) initIndex <- mkReg(0);

    // test state
    Reg#(TestState) testFSM <- mkReg(Init);

    // functions passed to DUT
    function Index getIndex(PipeCmd cmd);
        return cmd.index;
    endfunction

    function ActionValue#(TagMatchResult#(Way)) tagMatch(
        PipeCmd cmd, 
        Vector#(WayNum, Tag) tags, 
        Vector#(WayNum, MsiT) css, 
        Vector#(WayNum, Owner) owners,
        RepInfo repInfo
    );
    actionvalue
        if(testFSM == Process) begin
        end
        else begin
            $fwrite(stderr, "[TbPipe] ERROR: tagMatch called in ", fshow(testFSM), "\n");
            $finish;
        end
        // record ram output at tag match stage
        tmInfo.wset(TagMatchInfo {
            cmd: cmd,
            tagVec: tags,
            csVec: css,
            ownerVec: owners,
            repInfo: repInfo
        });
        return TagMatchResult {
            way: cmd.way,
            pRqMiss: cmd.pRqMiss
        };
    endactionvalue;
    endfunction

    function ActionValue#(Dir) updateChildDir(PipeCmd cmd, MsiT s, Dir dir);
    actionvalue
        return dir + zeroExtend(s);
    endactionvalue
    endfunction

    function Action checkPRsData(MsiT cs, Bool dataV);
        return noAction;
    endfunction

    function Action checkCRsData(PipeCmd cmd, Dir dir, Bool dataV);
        return noAction;
    endfunction

    function ActionValue#(RepInfo) updateReplacement(RepInfo r, Way w);
    actionvalue
        RepInfo newInfo = r + zeroExtend(w);
        updateRepInfo.wset(UpdateRepInfo {
            oldInfo: r,
            way: w,
            newInfo: newInfo
        });
        return newInfo;
    endactionvalue
    endfunction

    // RAMs
    Vector#(WayNum, RWBramCore#(Index, InfoType)) infoRam <- replicateM(mkRWBramCore);
    Vector#(WayNum, RWBramCore#(Index, Line)) dataRam <- replicateM(mkRWBramCore);
    RWBramCore#(Index, RepInfo) repRam <- mkRWBramCore;
    // DUT
    ReadOnly#(Bool) initDone = (interface ReadOnly;
        method Bool _read = testFSM != Init;
    endinterface);
    CCPipe#(WayNum, Index, Tag, MsiT, Dir, Owner, Other, RepInfo, Line, PipeCmd) dutPipe <- mkCCPipe(
        initDone, getIndex, tagMatch, updateChildDir, checkPRsData, checkCRsData, updateReplacement,
        infoRam, repRam, dataRam
    );

    // log files
    Reg#(File) enqLog <- mkReg(InvalidFile);
    Reg#(File) tmLog <- mkReg(InvalidFile); // tag match
    Reg#(File) updateRepLog <- mkReg(InvalidFile);
    Reg#(File) outLog <- mkReg(InvalidFile);
    Reg#(File) deqLog <- mkReg(InvalidFile);
    Reg#(File) checkLog <- mkReg(InvalidFile); // final ram value

    rule doInit(testFSM == Init);
        for(Integer i = 0; i < valueOf(WayNum); i = i+1) begin
            infoRam[i].wrReq(initIndex, unpack(0));
            dataRam[i].wrReq(initIndex, 0);
        end
        repRam.wrreq(initIndex, 0);
        if(initIndex < fromInteger(valueOf(IndexNum) - 1)) begin
            initIndex <= initIndex + 1;
        end
        else begin
            // init randomizer
            randEnq.cntrl.init;
            randInIndex.cntrl.init;
            randInWay.cntrl.init;
            randInMiss.cntrl.init;
            randInData.cntrl.init;
            randLineValid.cntrl.init;
            randRespLine.cntrl.init;
            randToStateValid.cntrl.init;
            randRespState.cntrl.init;
            randSwapDeq.cntrl.init;
            randSwapData.cntrl.init;
            randWrTag.cntrl.init;
            randWrCs.cntrl.init;
            randWrDir.cntrl.init;
            randWrOwner.cntrl.init;
            randWrOther.cntrl.init;
            randWrUpdateRep.cntrl.init;
            randWrLine.cntrl.init;
            // create log files
            File f <- $fopen("enq.log", "w");
            enqLog <= f;
            f <- $fopen("tm.log", "w");
            tmLog <= f;
            f <- $fopen("updateRep.log", "w");
            updateRepLog <= f;
            f <- $fopen("deq.log", "w");
            deqLog <= f;
            f <- $fopen("out.log", "w");
            outLog <= f;
            f <- $fopen("check.log", "w");
            checkLog <= f;
            // change state
            testFSM <= Process;
            $display("INFO: init done");
        end
    endrule

    rule sendPipeCmd(testFSM == Process && sendCnt < fromInteger(valueOf(TestNum)));
        // randomize cmd
        let index <- randInIndex.next;
        let way <- randInWay.next;
        let miss <- randInMiss.next;
        let data <- randInData.next;
        let cmd = PipeCmd {
            id: truncate(sendCnt),
            index: index,
            way: way,
            pRqMiss: miss,
            data: data
        };
        // randomize resp line
        let lv <- randLineValid.next;
        let line <- randRespLine.next;
        let respLine = getRespLine(lv, line);
        // randomize resp state
        let sv <- randToStateValid.next;
        let state  <- randRespState.next;
        let toState = getToState(sv, state);
        // whether we do enq now
        let enq <- randEnq.next;
        if(getEnq(enq)) begin
            dutPipe.enq(cmd, respLine, toState);
            enqInfo[0] <= Valid (EnqInfo {
                cmd: cmd,
                respLine: respLine,
                toState: toState
            });
        end
    endrule

    rule recvPipeOut(testFSM == Process && recvCnt < fromInteger(valueOf(TestNum)));
        // randomize ram write
        let tag <- randWrTag.next;
        let cs <- randWrCs.next;
        let dir <- randWrDir.next;
        let owner <- randWrOwner.next;
        let line <- randWrLine.next;
        RamDataType wrRam = RamData {
            info: CacheInfo {
                tag: tag,
                cs: cs,
                dir: dir,
                owner: owner
            },
            line: line
        };
        // randomize new cmd (just modify data field)
        let data <- randSwapData.next;
        let cmd = dutPipe.first.cmd;
        cmd.data = data;
        cmd.pRqMiss = False; // must reset to False
        // randomize swap/deq action
        let swapDeq <- randSwapDeq.next;
        // apply to pipe and record actions
        case(getSwapDeq(swapDeq))
            Deq: begin
                dutPipe.deqWrite(Invalid, wrRam);
                deqInfo[0] <= Valid (DeqInfo {
                    newCmd: Invalid,
                    wrRam: wrRam
                });
            end
            Swap: begin
                dutPipe.deqWrite(Valid (cmd), wrRam);
                deqInfo[0] <= Valid (DeqInfo {
                    newCmd: Valid (cmd),
                    wrRam: wrRam
                });
            end
        endcase
        // record pipe out
        pipeOut[0] <= Valid (dutPipe.first);
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule conProcess(testFSM == Process);
        Vector#(MaxPending, EnqInfo) pendReqNext = pendReq;

        // apply deq effect
        if(deqInfo[1] matches tagged Valid .deq) begin
            // do deq or swap
            if(!isValid(deq.newCmd)) begin
                // deq
                recvCnt <= recvCnt + 1;
            end
            else begin
                // swap: reset resp info
                EnqInfo e = pendReq[getPendIdx(recvCnt)];
                e.respLine = Invalid;
                e.toState = Invalid;
                e.cmd = fromMaybe(?, deq.newCmd);
                pendReqNext[getPendIdx(recvCnt)] = e;
            end
            // apply write effect
            let cmd = pendReq[getPendIdx(recvCnt)].cmd;
            refInfoRam[cmd.index][cmd.way] <= deq.wrRam.info;
            refDataRam[cmd.index][cmd.way] <= deq.wrRam.line;
            // write to log
            $fwrite(deqLog, "time: %t", $time,
                "\ndeq/swap: ", fshow(deq.newCmd),
                "\nindex: %d, way: %d", cmd.index, cmd.way,
                "\ntag: ", fshow(deq.wrRam.info.tag),
                "\ncs: ", fshow(deq.wrRam.info.cs),
                "\ndir: ", fshow(deq.wrRam.info.dir),
                "\nowner: ", fshow(deq.wrRam.info.owner),
                "\nline: ", fshow(deq.wrRam.line), "\n\n"
            );
        end
        // apply enq effect
        if(enqInfo[1] matches tagged Valid .e) begin
            pendReqNext[getPendIdx(sendCnt)] = e;
            sendCnt <= sendCnt + 1;
            $fwrite(enqLog, "time: %t, send %d", $time, sendCnt,
                "\ncmd: ", fshow(e.cmd),
                "\nrespLine: ", fshow(e.respLine),
                "\ntoState: ", fshow(e.toState), "\n\n"
            );
        end
        // write reg
        pendReq <= pendReqNext;
        // check tag match
        if(tmInfo[1] matches tagged Valid .tm) begin
            // get ref
            EnqInfo refEnq = pendReq[getPendIdx(zeroExtend(tm.cmd.id))];
            Index refIdx = refEnq.cmd.index;
            Vector#(WayNum, InfoType) refInfo = readVReg(refInfoRam[refIdx]);
            TagMatchInfo refTm = ?;
            refTm.cmd = refEnq.cmd;
            for(Integer i = 0; i < valueOf(WayNum); i = i+1) begin
                refTm.tagVec[i] = refInfo[i].tag;
                refTm.csVec[i] = refInfo[i].cs;
                refTm.ownerVec[i] = refInfo[i].owner;
            end
            if(deqInfo[1] matches tagged Valid .deq) begin
                let deqCmd = pendReq[getPendIdx(recvCnt)].cmd;
                let wrIdx = deqCmd.index;
                let wrWay = deqCmd.way;
                if(wrIdx == refIdx) begin
                    refTm.tagVec[wrWay] = deq.wrRam.info.tag;
                    refTm.csVec[wrWay] = deq.wrRam.info.cs;
                    refTm.ownerVec[wrWay] = deq.wrRam.info.owner;
                end
            end
            // write log
            $fwrite(tmLog, "time %t:\n", $time);
            $fwrite(tmLog, "ref: cmd: ", fshow(refTm.cmd),
                "\n     tag: ", fshow(refTm.tagVec),
                "\n     state: ", fshow(refTm.csVec),
                "\n     owner: ", fshow(refTm.ownerVec), "\n"
            );
            $fwrite(tmLog, "dut: cmd: ", fshow(tm.cmd),
                "\n     tag: ", fshow(tm.tagVec),
                "\n     state: ", fshow(tm.csVec),
                "\n     owner: ", fshow(tm.ownerVec), "\n"
            );
            // compare & check
            if(tm == refTm) begin
            end
            else begin
                $fwrite(stderr, "[TbPipe] ERROR: tm mismatch\n");
                $finish;
            end
        end
        // check output
        if(pipeOut[1] matches tagged Valid .out) begin
            // get ref
            EnqInfo refEnq = pendReq[getPendIdx(recvCnt)];
            Index refIdx = refEnq.cmd.index;
            Way refWay = refEnq.cmd.way;
            Bool refMiss = refEnq.cmd.pRqMiss;
            PipeOutType refOut = PipeOut {
                cmd: refEnq.cmd,
                way: refWay,
                pRqMiss: refMiss,
                ram: RamData {
                    info: refInfoRam[refIdx][refWay],
                    line: refDataRam[refIdx][refWay]
                }
            };
            if(refEnq.respLine matches tagged Valid .l) begin
                refOut.ram.line = l;
            end
            if(refEnq.toState matches tagged UpCs .s) begin
                refOut.ram.info.cs = s;
            end
            else if(refEnq.toState matches tagged DownDir .s) begin
                refOut.ram.info.dir <- updateChildDir(refOut.cmd, s, refOut.ram.info.dir);
            end
            // write log
            $fwrite(outLog, "time %t: recv %d\n", $time, recvCnt);
            $fwrite(outLog, "ref: cmd: ", fshow(refOut.cmd),
                "\n     way: ", fshow(refOut.way),
                "\n     pRqMiss: ", fshow(refOut.pRqMiss),
                "\n     tag: ", fshow(refOut.ram.info.tag),
                "\n     cs: ", fshow(refOut.ram.info.cs),
                "\n     dir: ", fshow(refOut.ram.info.dir),
                "\n     owner: ", fshow(refOut.ram.info.owner),
                "\n     line: ", fshow(refOut.ram.line), "\n"
            );
            $fwrite(outLog, "dut: cmd: ", fshow(out.cmd),
                "\n     way: ", fshow(out.way),
                "\n     pRqMiss: ", fshow(out.pRqMiss),
                "\n     tag: ", fshow(out.ram.info.tag),
                "\n     cs: ", fshow(out.ram.info.cs),
                "\n     dir: ", fshow(out.ram.info.dir),
                "\n     owner: ", fshow(out.ram.info.owner),
                "\n     line: ", fshow(out.ram.line), "\n\n"
            );
            // compare & check
            if(out == refOut) begin
            end
            else begin
                $fwrite(stderr, "[TbPipe] ERROR: out mismatch\n");
                $finish;
            end
        end
        // change state
        if(sendCnt == fromInteger(valueOf(TestNum)) && recvCnt == fromInteger(valueOf(TestNum))) begin
            testFSM <= Check;
            $display("INFO: process done");
        end
        // reset EHRs
        enqInfo[1] <= Invalid;
        deqInfo[1] <= Invalid;
        tmInfo[1] <= Invalid;
        pipeOut[1] <= Invalid;
    endrule

    // check ram values
    rule checkRam_readReq(testFSM == Check && !waitRamOut);
        for(Integer i = 0; i < valueOf(WayNum); i = i+1) begin
            dataRam[i].rdReq(checkIndex);
            infoRam[i].rdReq(checkIndex);
        end
        waitRamOut <= True;
    endrule

    rule checkRam_readResp(testFSM == Check && waitRamOut);
        for(Integer i = 0; i < valueOf(WayNum); i = i+1) begin
            // dut
            dataRam[i].deqRdResp;
            infoRam[i].deqRdResp;
            let line = dataRam[i].rdResp;
            let info = infoRam[i].rdResp;
            // ref
            Line refLine = refDataRam[checkIndex][i];
            InfoType refInfo = refInfoRam[checkIndex][i];
            // write log
            $fwrite(checkLog, "index: %d, way: %d\n", checkIndex, i);
            $fwrite(checkLog, "ref:",
                "\n    tag: ", fshow(refInfo.tag),
                "\n    cs: ", fshow(refInfo.cs),
                "\n    dir: ", fshow(refInfo.dir),
                "\n    owner: ", fshow(refInfo.owner),
                "\n    line: ", fshow(refLine), "\n"
            );
            $fwrite(checkLog, "dut:",
                "\n    tag: ", fshow(info.tag),
                "\n    cs: ", fshow(info.cs),
                "\n    dir: ", fshow(info.dir),
                "\n    owner: ", fshow(info.owner),
                "\n    line: ", fshow(line), "\n\n"
            );
            // compare & check
            if(refInfo == info && refLine == line) begin
            end
            else begin
                $fwrite(stderr, "[TbPipe] ERROR: ram final value mismatch\n");
                $finish;
            end
        end
        // change state
        if(checkIndex < fromInteger(valueOf(IndexNum) - 1)) begin
            checkIndex <= checkIndex + 1;
            waitRamOut <= False;
        end
        else begin
            $display("INFO: Pass");
            $finish;
        end
    endrule
endmodule
