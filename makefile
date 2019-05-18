all : main

main : main.o
	gcc -m32 -Wall -g main.o -o main 

main.o : main.s
	nasm -f elf main.s -o main.o 

.PHONY: clean
clean: 
	rm -f *.o calc.o calc





