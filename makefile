all : main

main : main.o 
	gcc -m32 -Wall -g main.o -o main 

# scheduler: scheduler.o
# 	gcc -m32 -Wall -g scheduler.o -o scheduler

main.o : main.s
	nasm -f elf -g main.s -o main.o 

# scheduler.o : scheduler.s
# 	nasm -f elf scheduler.s -o scheduler.o 


.PHONY: clean
clean: 
	rm -f *.o calc.o main main.o scheduler scheduler.o





