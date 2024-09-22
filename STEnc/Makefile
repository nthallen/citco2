LDFLAGS=-Wl,-rpath -Wl,/usr/local/lib -L /usr/local/lib
CPPFLAGS= -I /usr/local/include
CXXFLAGS= -Wall -g
LIBS=-ltmpp -ltm -lnort

SRC = STEnc_main.cc STEnc.cc STEnc_cmd.cc CmdReq.cc STEncoui.c
OBJ = ${addsuffix .o, ${basename $(SRC) } } 
TGT = STEnc

$(TGT) : $(OBJ)
	$(LINK.cc) -o $(TGT) $(OBJ) $(LIBS)
	usemsg $(TGT) STEncoui.c
STEncoui.c : STEnc.oui
	oui -o STEncoui.c STEnc.oui
.PHONY : clean
clean :
	rm -f *.o STEncoui.c STEnc
