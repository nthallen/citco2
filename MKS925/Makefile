include /usr/local/share/monarch/appgen.mk

CXXFLAGS+=-g -Wall -std=c++11
LDFLAGS=$(AG_LDFLAGS) -ldasio -lnl -lpthread

.PHONY : all
all : MKS925
OBJ = *.stackdump *.o
# TGT = $(tgt):/home/flight

MKS925OBJ = mks925.o mks925oui.o
OBJ += MKS925 mks925oui.cc
DIST += MKS925
MKS925 : $(MKS925OBJ) $(AG_LIBDIR)/libdasio.a
	$(CXX) -o MKS925 $(CXXFLAGS) $(MKS925OBJ) $(LDFLAGS)
mks925.o : mks925.cc
mks925oui.o : mks925oui.cc
mks925oui.cc : mks925.oui
	oui -o mks925oui.cc $<

clean :
	rm -f $(OBJ)
