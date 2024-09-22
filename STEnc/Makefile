LDFLAGS=-Wl,-rpath -Wl,/usr/local/lib -L /usr/local/lib
CPPFLAGS= -I /usr/local/include
CXXFLAGS= -Wall -g
LIBS=-ldasio -lnl -luldaq

SRC = STEnc_main.cc STEnc.cc STEnc_cmd.cc STEncoui.cc
OBJ = ${addsuffix .o, ${basename $(SRC) } } 
TGT = STEnc

$(TGT) : $(OBJ)
	$(LINK.cc) -o $(TGT) $(OBJ) $(LIBS)
STEncoui.cc : STEnc.oui
	oui -o STEncoui.cc STEnc.oui
.PHONY : clean
clean :
	rm -f *.o STEncoui.cc STEnc
