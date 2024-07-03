# This one probably needs some auto-configuration
# Specifically CPPFLAGS and LDFLAGS will have to be tweaked on 
# oco-fts.
CPPFLAGS = -I /usr/include/libxml2
CPPFLAGS += -I /usr/local/include
#LDLIBS = -lcurllog -lcurl -lxml2 -lnort -lrt
LDLIBS = -lcurllog -lcurl -lxml2 -ldasio -lnl
LDFLAGS = -L/usr/local/lib -Wl,-rpath -Wl,/usr/pkg/lib -Wl,-rpath -Wl,/usr/local/lib
OBJS = SunTracker.o STPort.o ST_Pkt.o STcmd.o Set_Time.o SToui.o Set_Mode.o Read_Top.o
CXXFLAGS = -Wall -g -O2

SunTracker : $(OBJS)
	$(LINK.cc) -o SunTracker $(OBJS) $(LDLIBS)
SToui.o : SToui.cc
SToui.cc : ST.oui
	oui -o SToui.cc ST.oui
clean :
	rm -f *.o SunTracker SunTimeTest
STtest : STtest.o STtestoui.o
	$(LINK.cc) -o STtest STtest.o STtestoui.o $(LDLIBS)
	usemsg STtest STtestoui.c
STtest.o : STtest.cc
STtestoui.o : STtestoui.c
STtestoui.c : STtest.oui
	oui -o STtestouic STtest.oui
