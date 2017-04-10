// L1 connect to Dir

// cache size = bank num * way num * set num * 64B

// 32KB L1
typedef 4 L1Num;

typedef 4 L1WayNum;
typedef 0 LgL1BankNum;
typedef 7 LgL1SetNum;

typedef Bit#(LgL1BankNum) L1BankId;
typedef LgL1SetNum L1IndexSz;
typedef Bit#(L1IndexSz) L1Index;
typedef GetTagSz#(LgL1BankNum, LgL1SetNum) L1TagSz;
typedef Bit#(L1TagSz) L1Tag;
typedef Bit#(TLog#(L1WayNum)) L1Way;

typedef 4 L1CRqNum;
typedef 2 L1PRqNum;
typedef Bit#(TLog#(L1CRqNum)) L1CRqMshrIdx;
typedef Bit#(TLog#(L1PRqNum)) L1PRqMshrIdx;

typedef Bit#(32) ProcRqId;

// Dir for 4MB memory (shorten simulation time)
typedef 22 LgMemSzBytes;
typedef 0 LgDirBankNum;
typedef 8 DirCRqNum;

typedef TSub#(LgMemSzBytes, TAdd#(LgLineSzBytes, LgDirBankNum)) DirIndexSz;
typedef Bit#(TLog#(DirCRqNum)) DirCRqMshrIdx;

typedef L1Num DirChildNum;
typedef Bit#(TLog#(DirChildNum)) DirChild;
typedef L1Way DirCRqId;

