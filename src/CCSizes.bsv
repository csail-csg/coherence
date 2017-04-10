import Types::*;
import CCTypes::*;
import FShow::*;

`ifdef CCSIZES_FILE
`include `CCSIZES_FILE
`else
staticAssert(False, "ERROR: CCSIZES_FILE undefined");
`endif

// Memory
//typedef 0 LgMemBankNum;
//typedef 1 MemChildNum; // number of child for each bank
//typedef Bit#(TLog#(MemChildNum)) MemChild;
//
//typedef 8 MemLdQSz;
//typedef 4 MemWrBSz;
//
//typedef Bit#(TLog#(DirCRqNum)) MemLdRqId; // XXX assume Mem connects to Dir
