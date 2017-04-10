import Vector::*;
import CCTypes::*;

interface RandomReplace#(numeric type wayNum);
    // find a way to replace, which is not locked
    // and Invalid way has priority
    method Maybe#(Bit#(TLog#(wayNum))) getReplaceWay(
        Vector#(wayNum, Bool) unlocked, 
        Vector#(wayNum, Bool) invalid
    );
endinterface

module mkRandomReplace(RandomReplace#(wayNum)) provisos(
    Alias#(wayT, Bit#(TLog#(wayNum)))
);
    Reg#(wayT) randWay <- mkReg(0);

    rule tick;
        randWay <= randWay == fromInteger(valueOf(wayNum) - 1) ? 0 : randWay + 1;
    endrule
    
    method Maybe#(wayT) getReplaceWay(Vector#(wayNum, Bool) unlocked, Vector#(wayNum, Bool) invalid);
        // first search for invalid & unlocked way
        function Bool isInvUnlock(Integer i);
            return unlocked[i] && invalid[i];
        endfunction
        Vector#(wayNum, Integer) idxVec = genVector;
        Maybe#(wayT) repWay = searchIndex(isInvUnlock, idxVec);
        if(!isValid(repWay)) begin
            // check whether random way is unlocked
            if(unlocked[randWay]) begin
                repWay = Valid (randWay);
            end
            else begin
                // just find a unlocked way
                repWay = searchIndex(id, unlocked);
            end
        end
        return repWay;
    endmethod
endmodule
