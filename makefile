# Variables
SRC=$(file)
OUT=$(SRC:.p=)
ASM=$(SRC:.p=.s)

# règles
all: $(OUT)

clean:
	rm -f *.o *.s compilateur tokeniser.cpp $(OUT)

tokeniser.cpp: tokeniser.l
	flex++ -d -otokeniser.cpp tokeniser.l

tokeniser.o: tokeniser.cpp
	g++ -c tokeniser.cpp

compilateur: compilateur.cpp tokeniser.o
	g++ -ggdb -o compilateur compilateur.cpp tokeniser.o

$(ASM): compilateur $(SRC)
	./compilateur <$(SRC) >$(ASM)

$(OUT): $(ASM)
	gcc -ggdb -no-pie -fno-pie $(ASM) -o $(OUT)

.PHONY: all clean
