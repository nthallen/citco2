LDFLAGS=-Wl,-rpath -Wl,/usr/local/lib -L /usr/local/lib
CPPFLAGS=-I /usr/local/include
CFLAGS=-Wall -g
CXXFLAGS=-Wall -g
LIBS=-ldasio -lnl

.PHONY : all clean

OBJ=LN2.o LN2oui.o
LN2 : $(OBJ)
	$(LINK.cc) -o LN2 $(OBJ) $(LIBS)
LN2oui.cc : LN2.oui
	oui -o LN2oui.cc LN2.oui
all : LN2
clean :
	rm LN2 LN2oui.cc *.o