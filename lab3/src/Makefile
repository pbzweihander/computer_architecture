buildbase = ../build
bdir = $(buildbase)/bdir
bindir = $(buildbase)/bin
simdir = $(buildbase)/simdir
libbase = ../lib

compile:
	mkdir -p $(simdir)
	mkdir -p $(bdir)
	mkdir -p $(bindir)

fold: compile
	rm -rf $(bdir)/mkTbFftFolded* $(simdir)/* $(bindir)/sim_fold.so $(bindir)/sim_fold $(libbase)/*.o
	bsc -u -sim -aggressive-conditions -D Folded -bdir $(bdir) -simdir $(simdir) -vdir $(buildbase) -p $(libbase):+ $(libbase)/TestBench.bsv
	bsc -u -sim -aggressive-conditions -D Folded -bdir $(bdir) -simdir $(simdir) -p $(libbase):+ -o $(bindir)/sim_fold -e mkTbFftFolded $(bdir)/*.ba $(libbase)/*.cpp

pipe: compile
	rm -rf $(bdir)/mkTbFftPipelined* $(simdir)/* $(bindir)/sim_pipe.so $(bindir)/sim_pipe $(libbase)/*.o
	bsc -u -sim -aggressive-conditions -D Pipelined -bdir $(bdir) -simdir $(simdir) -vdir $(buildbase) -p $(libbase):+ $(libbase)/TestBench.bsv
	bsc -u -sim -aggressive-conditions -D Pipelined -e mkTbFftPipelined -bdir $(bdir) -simdir $(simdir) -p $(libbase):+ -o $(bindir)/sim_pipe -e mkTbFftPipelined $(bdir)/*.ba $(libbase)/*.cpp

sfol1: compile
	rm -rf $(bdir)/mkTbFftSuperFolded1* $(simdir)/* $(bindir)/sim_sfol1.so $(bindir)/sim_sfol1 $(libbase)/*.o
	bsc -u -sim -aggressive-conditions -D SuperFolded1 -bdir $(bdir) -simdir $(simdir) -vdir $(buildbase) -p $(libbase):+ $(libbase)/TestBench.bsv
	bsc -u -sim -aggressive-conditions -D SuperFolded1 -e mkTbFftSuperFolded1 -bdir $(bdir) -simdir $(simdir) -p $(libbase):+ -o $(bindir)/sim_sfol1 -e mkTbFftSuperFolded1 $(bdir)/*.ba $(libbase)/*.cpp

sfol2: compile
	rm -rf $(bdir)/mkTbFftSuperFolded2* $(simdir)/* $(bindir)/sim_sfol2.so $(bindir)/sim_sfol2 $(libbase)/*.o
	bsc -u -sim -aggressive-conditions -D SuperFolded2 -bdir $(bdir) -simdir $(simdir) -vdir $(buildbase) -p $(libbase):+ $(libbase)/TestBench.bsv
	bsc -u -sim -aggressive-conditions -D SuperFolded2 -e mkTbFftSuperFolded2 -bdir $(bdir) -simdir $(simdir) -p $(libbase):+ -o $(bindir)/sim_sfol2 -e mkTbFftSuperFolded2 $(bdir)/*.ba $(libbase)/*.cpp

sfol4: compile
	rm -rf $(bdir)/mkTbFftSuperFolded4* $(simdir)/* $(bindir)/sim_sfol4.so $(bindir)/sim_sfol4 $(libbase)/*.o
	bsc -u -sim -aggressive-conditions -D SuperFolded4 -bdir $(bdir) -simdir $(simdir) -vdir $(buildbase) -p $(libbase):+ $(libbase)/TestBench.bsv
	bsc -u -sim -aggressive-conditions -D SuperFolded4 -e mkTbFftSuperFolded4 -bdir $(bdir) -simdir $(simdir) -p $(libbase):+ -o $(bindir)/sim_sfol4 -e mkTbFftSuperFolded4 $(bdir)/*.ba $(libbase)/*.cpp

sfol8: compile
	rm -rf $(bdir)/mkTbFftSuperFolded8* $(simdir)/* $(bindir)/sim_sfol8.so $(bindir)/sim_sfol8 $(libbase)/*.o
	bsc -u -sim -aggressive-conditions -D SuperFolded8 -bdir $(bdir) -simdir $(simdir) -vdir $(buildbase) -p $(libbase):+ $(libbase)/TestBench.bsv
	bsc -u -sim -aggressive-conditions -D SuperFolded8 -e mkTbFftSuperFolded8 -bdir $(bdir) -simdir $(simdir) -p $(libbase):+ -o $(bindir)/sim_sfol8 -e mkTbFftSuperFolded8 $(bdir)/*.ba $(libbase)/*.cpp

sfol16: compile
	rm -rf $(bdir)/mkTbFftSuperFolded16* $(simdir)/* $(bindir)/sim_sfol16.so $(bindir)/sim_sfol16 $(libbase)/*.o
	bsc -u -sim -aggressive-conditions -D SuperFolded16 -bdir $(bdir) -simdir $(simdir) -vdir $(buildbase) -p $(libbase):+ $(libbase)/TestBench.bsv
	bsc -u -sim -aggressive-conditions -D SuperFolded16 -e mkTbFftSuperFolded16 -bdir $(bdir) -simdir $(simdir) -p $(libbase):+ -o $(bindir)/sim_sfol16 -e mkTbFftSuperFolded16 $(bdir)/*.ba $(libbase)/*.cpp

all: compile fold pipe sfol1 sfol2 sfol4 sfol8 sfol16

clean:
	rm -rf $(buildbase)

.PHONY: clean fold pipe sfol sfol1 sfol2 sfol4 sfol8 sfol16 all
.DEFAULT_GOAL := all
