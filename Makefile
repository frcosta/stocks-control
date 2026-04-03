all: stocks.exe

stocks.exe: stocks.cbl
	cobc -x stocks.cbl fmsgs.cbl procustody.cbl valdata.cbl -o stocks.exe

%.exe: %.cbl
	cobc -x $< -o $@

clean:
	rm -f *.exe *.o

.PHONY: all clean

