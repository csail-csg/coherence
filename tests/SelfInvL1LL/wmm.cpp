#include <vector>
#include <queue>
#include <string.h>
#include <stdint.h>
#include <stdio.h>

// 64B line data
class Line {
public:
    static const int data_num = 8; // double word
    static const int byte_num = 64;

    Line() {
        memset(data, 0, byte_num); 
    }
    Line(const Line &x) {
        memcpy(data, x.data, byte_num);
    }
    Line(const void *p) {
        memcpy(data, p, byte_num);
    }
    Line& operator=(const Line &x) {
        memcpy(data, x.data, byte_num);
        return *this;
    }
    
    inline bool equal(const void *p) {
        const uint64_t *x = (const uint64_t*)p;
        return (data[0] == x[0] &&
                data[1] == x[1] &&
                data[2] == x[2] &&
                data[3] == x[3] &&
                data[4] == x[4] &&
                data[5] == x[5] &&
                data[6] == x[6] &&
                data[7] == x[7]);
    }

    uint64_t data[8];
};

// invalidation buffer
class InvBuffer {
public:
    InvBuffer(uint32_t index_n, uint32_t tag_n) :
        index_num(index_n),
        tag_num(tag_n),
        ib(index_num, std::vector<std::queue<Line> >(tag_num))
    {
    }

    inline void push_stale(uint32_t index, uint32_t tag, const Line &line) {
        ib[index][tag].push(line);
    }

    // return true if the line is found in ib
    inline bool find_line(uint32_t index, uint32_t tag, const void *line) {
        std::queue<Line> &q = ib[index][tag];
        while (!q.empty()) {
            if (q.front().equal(line)) {
                return true; // find the line, succeed, stop
            }
            q.pop();
        }
        return false; // fail to find the line
    }

    // return true if the double-word data is found in ib
    inline int find_data(uint32_t index, uint32_t tag,
                         uint8_t sel, uint64_t data) {
        std::queue<Line> &q = ib[index][tag];
        while (!q.empty()) {
            if (q.front().data[sel] == data) {
                return true; // find the data, succeed, stop
            }
            q.pop();
        }
        return false; // fail to find the data
    }

    inline void clear_addr(uint32_t index, uint32_t tag) {
        std::queue<Line> &q = ib[index][tag];
        while (!q.empty()) q.pop();
    }

    inline void reconcile() {
        for (uint32_t index = 0; index < index_num; index++) {
            for (uint32_t tag = 0; tag < tag_num; tag++) {
                clear_addr(index, tag);
            }
        }
    }

private:
    const uint32_t index_num;
    const uint32_t tag_num;

    // ib[index][tag] is FIFO of stale cache-line values. FIFO front is most
    // stale.
    std::vector<std::vector<std::queue<Line> > > ib;
};

class WMMSys {
public:
    WMMSys(uint8_t core_n, uint32_t index_n, uint32_t tag_n) :
        core_num(core_n),
        index_num(index_n),
        tag_num(tag_n),
        ib(core_num, InvBuffer(index_num, tag_num)),
        mem(index_num, std::vector<Line>(tag_num))
    {
    }

    inline bool find_line(uint8_t core, uint32_t index, uint32_t tag,
                          const void *line) {
        if (ib[core].find_line(index, tag, line)) {
            return true;
        }
        return mem[index][tag].equal(line);
    }

    inline bool find_data(uint8_t core, uint32_t index, uint32_t tag,
                          uint8_t sel, uint64_t data) {
        if (ib[core].find_data(index, tag, sel, data)) {
            return true;
        }
        return mem[index][tag].data[sel] == data;
    }

    inline void read_mem_line(void *line, uint32_t index, uint32_t tag) {
        memcpy(line, mem[index][tag].data, Line::byte_num);
    }

    inline uint64_t read_mem_data(uint32_t index, uint32_t tag, uint8_t sel) {
        return mem[index][tag].data[sel];
    }

    inline void write_mem_line(uint32_t index, uint32_t tag,
                               const void *line) {
        memcpy(mem[index][tag].data, line, Line::byte_num);
    }

    inline void write_mem_data(uint32_t index, uint32_t tag,
                               uint8_t sel, uint64_t data) {
        mem[index][tag].data[sel] = data;
    }

    inline void clear_addr(uint8_t core, uint32_t index, uint32_t tag) {
        ib[core].clear_addr(index, tag);
    }

    inline void reconcile(uint8_t core) {
        ib[core].reconcile();
    }
    
    // value is modified by core, push stale values to all other cores
    inline void push_stale(uint8_t core, uint32_t index, uint32_t tag) {
        Line &line = mem[index][tag];
        for (uint8_t i = 0; i < core_num; i++) {
            if (i != core) {
                ib[i].push_stale(index, tag, line);
            }
        }
    }

    // value is modified by DMA, push stale values to all cores
    inline void push_stale(uint32_t index, uint32_t tag) {
        Line &line = mem[index][tag];
        for (uint8_t i = 0; i < core_num; i++) {
            ib[i].push_stale(index, tag, line);
        }
    }

private:
    const uint8_t core_num;
    const uint32_t index_num;
    const uint32_t tag_num;

    std::vector<InvBuffer> ib;
    std::vector<std::vector<Line> > mem;
};

static WMMSys *wmm = NULL;
static FILE *fp = NULL;

extern "C" void wmmInit(unsigned char core_num,
                        unsigned int index_num,
                        unsigned int tag_num) {
    fp = fopen("wmm.log", "w");
    fprintf(fp, "init: core %d index %d tag %d\n",
            int(core_num), index_num, tag_num);
    fflush(fp);
    wmm = new WMMSys(core_num, index_num, tag_num);
}

extern "C" unsigned char wmmFindLine(unsigned char core, unsigned int index,
                                     unsigned int tag, unsigned int *line) {
    fprintf(fp, "find line: core %d, index %d, tag %d",
            int(core), index, tag);
    fflush(fp);
    unsigned char found = wmm->find_line(core, index, tag, line) ? 1 : 0;
    fprintf(fp, ", found %d\n", int(found));
    fflush(fp);
    return found;
}

extern "C" unsigned char wmmFindData(unsigned char core,
                                     unsigned int index,
                                     unsigned int tag,
                                     unsigned char sel,
                                     unsigned long long data) {
    fprintf(fp, "find data: core %d, index %d, tag %d, sel %d, data %016llx",
            int(core), index, tag, sel, data);
    fflush(fp);
    unsigned char found = wmm->find_data(core, index, tag, sel, data) ? 1 : 0;
    fprintf(fp, ", found %d\n", int(found));
    fflush(fp);
    return found;
}

extern "C" void wmmReadMemLine(unsigned int *line,
                               unsigned int index,
                               unsigned int tag) {
    fprintf(fp, "read line: index %d, tag %d\n", index, tag);
    fflush(fp);
    wmm->read_mem_line(line, index, tag);
}

extern "C" unsigned long long wmmReadMemData(unsigned int index,
                                             unsigned int tag,
                                             unsigned char sel) {
    fprintf(fp, "read data: index %d, tag %d, sel %d",
            index, tag, sel);
    fflush(fp);
    uint64_t data = wmm->read_mem_data(index, tag, sel);
    fprintf(fp, ", data %016llx\n", (long long unsigned)data);
    fflush(fp);
    return data;
}

extern "C" void wmmWriteMemLine(unsigned int index,
                                unsigned int tag,
                                unsigned int *line) {
    fprintf(fp, "write line: index %d, tag %d\n", index, tag);
    fflush(fp);
    wmm->write_mem_line(index, tag, line);
}

extern "C" void wmmWriteMemData(unsigned int index,
                                unsigned int tag,
                                unsigned char sel,
                                unsigned long long data) {
    fprintf(fp, "write data: index %d, tag %d, sel %d, data %016llx\n",
            index, tag, sel, data);
    fflush(fp);
    wmm->write_mem_data(index, tag, sel, data);
}

extern "C" void wmmClearAddr(unsigned char core,
                             unsigned int index,
                             unsigned int tag) {
    fprintf(fp, "clear addr: core %d, index %d, tag %d\n",
            int(core), index, tag);
    fflush(fp);
    wmm->clear_addr(core, index, tag);
}

extern "C" void wmmReconcile(unsigned char core) {
    fprintf(fp, "reconcile: core %d\n", int(core));
    fflush(fp);
    wmm->reconcile(core);
}

extern "C" void wmmPushStaleByCore(unsigned char core,
                                   unsigned int index,
                                   unsigned int tag) {
    fprintf(fp, "stale by core: core %d, index %d, tag %d\n",
            int(core), index, tag);
    fflush(fp);
    wmm->push_stale(core, index, tag);
}

extern "C" void wmmPushStaleByDma(unsigned int index,
                                  unsigned int tag) {
    fprintf(fp, "stale by dma: index %d, tag %d\n", index, tag);
    fflush(fp);
    wmm->push_stale(index, tag);
}

extern "C" void wmmFinish() {
    if (!fp) fclose(fp);
    if (!wmm) delete wmm;
    fp = NULL;
    wmm = NULL;
}
