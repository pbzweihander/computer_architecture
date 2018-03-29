buildbase = ../build
bdir = $(buildbase)/bdir
bindir = $(buildbase)/bin
simdir = $(buildbase)/simdir
libdir = ../lib

compile:
	mkdir -p $(simdir)
	mkdir -p $(bindir)
	mkdir -p $(bdir)

left: compile
	rm -rf $(bdir)/mkTbLeft* $(simdir)/* $(bindir)/sim_Left.so $(bindir)/sim_Left $(libbase)/*.o
	bsc -u -sim -aggressive-conditions -unspecified-to 'A' -simdir $(simdir) -bdir $(bdir) -p +:$(libdir) $(libdir)/TestBench.bsv
	bsc -u -sim -aggressive-conditions -unspecified-to 'A' -simdir $(simdir) -bdir $(bdir) -p $(libdir):+ -o $(bindir)/sim_Left -e mkTbLeft $(bdir)/*.ba $(libdir)/*.cpp

lp: compile
	rm -rf $(bdir)/mkTbLeftPipelined* $(simdir)/* $(bindir)/sim_LP.so $(bindir)/sim_LP $(libbase)/*.o
	bsc -u -sim -aggressive-conditions -simdir $(simdir) -bdir $(bdir) -p +:$(libdir) $(libdir)/TestBench.bsv
	bsc -u -sim -aggressive-conditions -simdir $(simdir) -bdir $(bdir) -p $(libdir):+ -o $(bindir)/sim_LP -e mkTbLeftPipelined $(bdir)/*.ba $(libdir)/*.cpp

rlp: compile
	rm -rf $(bdir)/mkTbRightLogicalPipelined* $(simdir)/* $(bindir)/sim_RLP.so $(bindir)/sim_RLP $(libbase)/*.o
	bsc -u -sim -aggressive-conditions -simdir $(simdir) -bdir $(bdir) -p +:$(libdir) $(libdir)/TestBench.bsv
	bsc -u -sim -aggressive-conditions -simdir $(simdir) -bdir $(bdir) -p $(libdir):+ -o $(bindir)/sim_RLP -e mkTbRightLogicalPipelined $(bdir)/*.ba $(libdir)/*.cpp

rap: compile
	rm -rf $(bdir)/mkTbRightArithmeticPipelined* $(simdir)/* $(bindir)/sim_RAP.so $(bindir)/sim_RAP $(libbase)/*.o
	bsc -u -sim -aggressive-conditions -simdir $(simdir) -bdir $(bdir) -p +:$(libdir) $(libdir)/TestBench.bsv
	bsc -u -sim -aggressive-conditions -simdir $(simdir) -bdir $(bdir) -p $(libdir):+ -o $(bindir)/sim_RAP -e mkTbRightArithmeticPipelined $(bdir)/*.ba $(libdir)/*.cpp

all: compile left lp rlp rap

clean:
	rm -rf $(buildbase)

.PHONY: clean all left lp rlp rap
.DEFAULT_GOAL := all
