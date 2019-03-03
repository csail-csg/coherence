
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

TARGETS = Pipe
#IntPipe L1Pipe LLPipe

MACROS = -D CCSIZES_FILE=\"L1L2LLSizes.bsv\"

LINK_JOBS ?= 8

$(TARGETS): %:
	mkdir -p build$@Dir
	bsc -u -sim \
		-bdir build$@Dir \
		-info-dir build$@Dir \
		-simdir build$@Dir \
		-vdir build$@Dir \
		+RTS -K100M -RTS \
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
		-Xc -std=c99 \
		-Xc++ -D_GLIBCXX_USE_CXX11_ABI=0 \
		-parallel-sim-link $(LINK_JOBS) \
		-o sim$@ build$@Dir/*.ba


all: Pipe

clean:
	rm -rf build*Dir sim* *.log ../include/*.o

.PHONY: clean all $(TARGETS)
.DEFAULT_GOAL := all
