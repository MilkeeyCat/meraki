build:
	nasm -f elf64 main.nasm
	ld -dynamic-linker /lib64/ld-linux-x86-64.so.2 /usr/lib64/crt1.o /usr/lib64/crti.o -lc main.o /usr/lib64/crtn.o -z noexecstack -o main

