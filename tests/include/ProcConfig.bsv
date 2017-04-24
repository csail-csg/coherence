`define CONNECTAL_MEMORY
`define IN_ORDER
`define rv64 True
`define m True
`define a True
`define f True
`define d True
`define sizeSup 2
// Set this define to use the FMA for Add and Mul
`define REUSE_FMA

// Defines to match spike's behavior
// `define CYCLE_COUNT_EQ_INST_COUNT
`define DISABLE_STIP
`define LOOK_LIKE_A_ROCKET

// Debugging infrastructure
`define VERIFICATION_PACKETS

// Workarounds
// `define WORKAROUND_ISSUE_27
// `define SERIALIZE_MEM_REQS
`define FLUSH_CACHES_ON_HTIF

`define NUM_CORES 8
`define NUM_EPOCHS 8
`define NUM_SPEC_TAGS 16
`define ROB_SIZE 8
`define BOOKKEEPING_FIFO_SIZE 4
`define LDSTQ_SIZE 8
`define SB_SIZE 4
`define AXI_ARBITER_FIFO_SIZE 16
`define DDR3LLC_MAX_READS 16
`define LOG_DEADLOCK_CYCLES 26

`define PHYS_REG_COUNT TAdd#(64,`ROB_SIZE)
