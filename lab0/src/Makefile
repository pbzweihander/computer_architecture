buildbase = ../build
bdir = $(buildbase)/bdir
bindir = $(buildbase)/bin
simdir = $(buildbase)/simdir

compile:
	mkdir -p $(bdir)
	mkdir -p $(bindir)
	mkdir -p $(simdir)

hello: compile
	rm -rf $(bdir)/mkHelloWorld* $(simdir)/* $(bindir)/test_hello.so $(bindir)/test_hello $(libbase)/*.o
	bsc -u -sim -aggressive-conditions -simdir $(simdir) -bdir $(bdir) ./HelloWorld.bsv
	bsc -u -sim -aggressive-conditions -simdir $(simdir) -bdir $(bdir) -o $(bindir)/test_hello -e mkHelloWorld

hihohiho: compile
	rm -rf $(bdir)/mkHiHoHiHo* $(simdir)/* $(bindir)/test_hihohiho.so $(bindir)/test_hihohiho $(libbase)/*.o
	bsc -u -sim -aggressive-conditions -simdir $(simdir) -bdir $(bdir) ./HiHoHiHo.bsv

	bsc -u -sim -aggressive-conditions -simdir $(simdir) -bdir $(bdir) -o $(bindir)/test_hihohiho -e mkHiHoHiHo
hihihoho: compile
	rm -rf $(bdir)/mkHiHiHoHo* $(simdir)/* $(bindir)/test_hihihoho.so $(bindir)/test_hihihoho $(libbase)/*.o
	bsc -u -sim -aggressive-conditions -simdir $(simdir) -bdir $(bdir) ./HiHiHoHo.bsv
	bsc -u -sim -aggressive-conditions -simdir $(simdir) -bdir $(bdir) -o $(bindir)/test_hihihoho -e mkHiHiHoHo

all: compile hello hihohiho hihihoho

clean:
	rm -rf $(buildbase)

.PHONY: hello hihihoho hihohiho clean all
.DEFAULT_GOAL := all
