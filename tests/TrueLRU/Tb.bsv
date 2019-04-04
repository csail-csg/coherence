import Vector::*;
import Types::*;
import CCTypes::*;
import LruReplace::*;
import Randomizable::*;
import Printf::*;

typedef 8 WayNum;
typedef Bit#(TLog#(WayNum)) Way;
typedef TrueLruRepInfo#(WayNum) RepInfo;

// reference update func
function RepInfo refUpdateRepInfo(RepInfo info, Way w);
    RepInfo newInfo = info;
    newInfo[0] = w;
    Bool found = False;
    for(Integer i = 0; i < valueof(WayNum) - 1; i = i+1) begin
        found = found || (info[i] == w);
        if(!found) begin
            newInfo[i + 1] = info[i];
        end
    end
    return newInfo;
endfunction

// check if rep info is still a permute of 0 .. waynum-1
function Bool isValidRepInfo(RepInfo info);
    Bool ret = True;
    Vector#(WayNum, Bool) found = replicate(False);
    for(Integer i = 0; i < valueof(WayNum); i = i+1) begin
        Way w = info[i];
        if(found[i]) begin
            ret = False;
        end
        found[i] = True;
    end
    if(found != replicate(True)) begin
        ret = False;
    end
    return ret;
endfunction

typedef 10000 TestNum;
typedef Bit#(TLog#(TestNum)) TestId;

(* synthesize *)
module mkTb(Empty);
    Randomize#(Way) randWay <- mkGenericRandomizer;
    Reg#(TestId) testId <- mkReg(0);
    Reg#(Bool) inited <- mkReg(False);

    Reg#(RepInfo) repInfo <- mkRegU;

    ReplacePolicy#(WayNum, RepInfo) repPolicy <- mkTrueLruReplace;

    rule doInit(!inited);
        randWay.cntrl.init;
        let rep = repPolicy.initRepInfo;
        $display("Init: ", fshow(rep));
        doAssert(isValidRepInfo(rep), "must be valid");
        repInfo <= rep;
        inited <= True;
    endrule

    rule doTest(inited);
        let way <- randWay.next;
        let dutRep = repPolicy.updateRepInfo(repInfo, way);
        let refRep = refUpdateRepInfo(repInfo, way);
        $display("Test %d: way %d, dut ", testId, way, fshow(dutRep), ", ref ", fshow(refRep));
        doAssert(isValidRepInfo(dutRep), "must be valid");
        doAssert(dutRep == refRep, "must match");
        repInfo <= dutRep;
        testId <= testId + 1;
        if(testId == fromInteger(valueof(TestNum) - 1)) begin
            $fdisplay(stderr, "PASS");
            $finish;
        end
    endrule
endmodule
