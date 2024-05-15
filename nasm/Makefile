build:
	nasm -f elf64 main.nasm
	ld -dynamic-linker /lib64/ld-linux-x86-64.so.2 /usr/lib64/crt1.o -lc main.o -z noexecstack -o main
