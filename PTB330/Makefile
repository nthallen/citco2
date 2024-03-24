include /usr/local/share/monarch/appgen.mk

CXXFLAGS+=-g -Wall -std=c++11
LDFLAGS=$(AG_LDFLAGS) -ldasio -lnl -lpthread

.PHONY : all
all : PTB330
OBJ = *.stackdump *.o
# TGT = $(tgt):/home/flight

PTB330OBJ = PTB330.o PTB330oui.o
OBJ += PTB330 PTB330oui.cc
DIST += PTB330
PTB330 : $(PTB330OBJ) $(AG_LIBDIR)/libdasio.a
	$(CXX) -o PTB330 $(CXXFLAGS) $(PTB330OBJ) $(LDFLAGS)
PTB330.o : PTB330.cc
PTB330oui.o : PTB330oui.cc
PTB330oui.cc : PTB330.oui
	oui -o PTB330oui.cc $<

clean :
	rm -f $(OBJ)
