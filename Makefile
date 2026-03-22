all: stocks.exe

stocks.exe: stocks.cbl sortfile.cbl
	cobc -x stocks.cbl sortfile.cbl fmsgs.cbl -o stocks.exe

%.exe: %.cbl
	cobc -x $< -o $@

clean:
	rm -f *.exe *.o

.PHONY: all clean

