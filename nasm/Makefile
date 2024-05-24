OBJ_PATH=/usr/lib/x86_64-linux-gnu

build:
	nasm -f elf64 main.nasm
	ld -dynamic-linker $(OBJ_PATH)/ld-linux-x86-64.so.2 $(OBJ_PATH)/crt1.o -lc main.o -z noexecstack -o main
