include /usr/local/share/monarch/appgen.mk

CXXFLAGS+=-g -Wall -std=c++11 -D_POSIX_C_SOURCE=1
LDFLAGS=$(AG_LDFLAGS) -ldasio -lnl -lpthread

.PHONY : all
all : webpower
OBJ = *.stackdump *.o
# TGT = $(tgt):/home/flight

WEBPOWEROBJ = webpower.o exec_child.o webpoweroui.o
OBJ += webpower webpoweroui.cc
DIST += webpower
webpower : $(WEBPOWEROBJ) $(AG_LIBDIR)/libdasio.a
	$(CXX) -o webpower $(CXXFLAGS) $(WEBPOWEROBJ) $(LDFLAGS)
webpower.o : webpower.cc
webpoweroui.o : webpoweroui.cc
webpoweroui.cc : webpower.oui
	oui -o webpoweroui.cc $<

clean :
	rm -f $(OBJ)
