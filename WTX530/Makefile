include /usr/local/share/monarch/appgen.mk

CXXFLAGS+=-g -Wall -std=c++11
LDFLAGS=$(AG_LDFLAGS) -ldasio -lnl -lpthread

.PHONY : all
all : WTX530
OBJ = *.stackdump *.o
# TGT = $(tgt):/home/flight

WTX530OBJ = WTX530.o WTX530oui.o
OBJ += WTX530 WTX530oui.cc
DIST += WTX530
WTX530 : $(WTX530OBJ) $(AG_LIBDIR)/libdasio.a
	$(CXX) -o WTX530 $(CXXFLAGS) $(WTX530OBJ) $(LDFLAGS)
WTX530.o : WTX530.cc
WTX530oui.o : WTX530oui.cc
WTX530oui.cc : WTX530.oui
	oui -o WTX530oui.cc $<

clean :
	rm -f $(OBJ)
