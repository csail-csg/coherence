
# Copyright (c) 2017 Massachusetts Institute of Technology
# 
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation
# files (the "Software"), to deal in the Software without
# restriction, including without limitation the rights to use, copy,
# modify, merge, publish, distribute, sublicense, and/or sell copies
# of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

EXTPATH = -p +:../include:../../src:../../../procs/lib
CFILES = ../include/atomic_mem.c

TARGETS = L1LL

STORE_PREFETCH ?= false

MACROS = -D CCSIZES_FILE=\"L1LLSizes.bsv\" -D DEBUG_DMA -D DEBUG_ICACHE \
		 -D NO_REQ_STALL -D NO_DMA_REQ_STALL \
		 -D BSIM

ifeq ($(STORE_PREFETCH),true)
MACROS += -D STORE_PREFETCH -D DEBUG_STORE_PREFETCH
endif

LINK_JOBS ?= 8

$(TARGETS): %:
	mkdir -p build$@Dir
	bsc -u -sim \
		-bdir build$@Dir \
		-info-dir build$@Dir \
		-simdir build$@Dir \
		-vdir build$@Dir \
		+RTS -K500M -RTS \
		-steps-warn-interval 1000000 \
		-show-schedule \
		-aggressive-conditions \
		-check-assert \
		$(MACROS) \
		$(EXTPATH) Tb$@.bsv
	grep -n "Blocking rules" build$@Dir/mkTb$@.sched | sed '/(none)/d'
	bsc -sim -e mkTb$@ \
		-bdir build$@Dir \
		-info-dir build$@Dir \
		-simdir build$@Dir \
		+RTS -K500M -RTS \
		-Xc -std=c99 \
		-Xc++ -D_GLIBCXX_USE_CXX11_ABI=0 \
		-parallel-sim-link $(LINK_JOBS) \
		-o sim$@ build$@Dir/*.ba $(CFILES)


all: $(TARGETS)


clean:
	rm -rf build*Dir sim* log *.log ../include/*.o

.PHONY: clean all $(TARGETS)
.DEFAULT_GOAL := all
