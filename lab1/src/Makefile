buildbase=../build
bdir=$(buildbase)/bdir
bindir=$(buildbase)/bin
simdir=$(buildbase)/simdir
libdir=../lib

compile:
	mkdir -p $(simdir)
	mkdir -p $(bindir)
	mkdir -p $(bdir)

mul: compile
	rm -rf $(bdir)/mkTbMultiplexer* $(simdir)/* $(bindir)/sim_mul.so $(bindir)/sim_mul $(libbase)/*.o
	bsc -u -sim -bdir $(bdir) -info-dir $(buildbase) -simdir $(simdir) -vdir $(buildbase) -aggressive-conditions -p +:$(libdir) ../lib/TestBench.bsv
	bsc -sim -e mkTbMultiplexer -bdir $(bdir) -info-dir $(buildbase) -simdir $(simdir) -o $(bindir)/sim_mul  $(libdir)/*.cpp

rl: compile
	rm -rf $(bdir)/mkTbRightLogical* $(simdir)/* $(bindir)/sim_rl.so $(bindir)/sim_rl $(libbase)/*.o
	bsc -u -sim -bdir $(bdir) -info-dir $(buildbase) -simdir $(simdir) -vdir $(buildbase) -aggressive-conditions -p +:$(libdir) ../lib/TestBench.bsv
	bsc -sim -e mkTbRightLogical -bdir $(bdir) -info-dir $(buildbase) -simdir $(simdir) -o $(bindir)/sim_rl  $(libdir)/*.cpp

ra: compile
	rm -rf $(bdir)/mkTbRightArith* $(simdir)/* $(bindir)/sim_ra.so $(bindir)/sim_ra $(libbase)/*.o
	bsc -u -sim -bdir $(bdir) -info-dir $(buildbase) -simdir $(simdir) -vdir $(buildbase) -aggressive-conditions -p +:$(libdir) ../lib/TestBench.bsv
	bsc -sim -e mkTbRightArith -bdir $(bdir) -info-dir $(buildbase) -simdir $(simdir) -o $(bindir)/sim_ra  $(libdir)/*.cpp

all: compile mul rl ra

clean:
	rm -rf $(buildbase)

.PHONY: clean all ra rl mul compile
.DEFAULT_GOAL := all
