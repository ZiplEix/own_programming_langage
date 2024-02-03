import sys
import os

program_path = sys.argv[1]

#############
# TOKENISER #
#############

# read line one by one
program_lines = []
with open(program_path, 'r') as program_file:
    program_lines = [line.strip() for line in program_file.readlines()]

tokens = []
for line in program_lines:
    parts = line.split(' ')
    opcode = parts[0]

    # check empty line
    if len(opcode) == 0:
        continue

    # store opcode token
    tokens.append(opcode)

    # handle each opcode
    if opcode == 'PUSH':
        # need a number after PUSH
        number = int(parts[1])
        tokens.append(number)
    elif opcode == 'PRINT':
        # parse string literal
        string_literal = ' '.join(parts[1:])[1:-1]
        tokens.append(string_literal)
    elif opcode == 'JUMP.EQ.0':
        etiquette = parts[1]
        tokens.append(etiquette)
    elif opcode == 'JUMP.GT.0':
        etiquette = parts[1]
        tokens.append(etiquette)

#############
# ASSEMBLER #
#############

string_literals = []
for ip in range(len(tokens)):
    if tokens[ip] == 'PRINT':
        string_literal = tokens[ip + 1]
        tokens[ip + 1] = len(string_literals)
        string_literals.append(string_literal)

asm_path = program_path.replace('.ocl', '.asm')
out = open(asm_path, 'w')

# write header
out.write("""; -- header --
bits 64
default rel
""")

# write bss section
out.write("""
; -- bss section --
section .bss
input_buffer resb 16 ; reserve 16 bytes for the input buffer
""")

# write data section
out.write("""
; -- data/constant section --
section .data
""")
for i, string_literal in enumerate(string_literals):
    out.write(f'string_literal_{i} db "{string_literal}", 0\n') # 0 is the null terminator | db indicates that we are writing a byte

# write text section
out.write("""
; -- Entry Point --
section .text
global _start

; -- Function to convert input to number --
; Assumes input is in 'input_buffer'
; Result will be in RAX
convert_input_to_number:
	mov rsi, input_buffer  ; Input string address
	xor rax, rax           ; Clear RAX for result
convert_loop:
	movzx rcx, byte [rsi]  ; Load next byte
	test rcx, rcx          ; Check for null terminator
	jz convert_done
	sub rcx, '0'           ; Convert from ASCII to integer
	imul rax, rax, 10      ; Multiply current result by 10
	add rax, rcx           ; Add new digit
	inc rsi                ; Move to next character
	jmp convert_loop
convert_done:
	ret

_start:
; shadow space (microsoft calling convention) (min 32 bytes)
\tpush rbp
\tmov rbp, rsp
\tsub rsp, 32

""")

ip = 0
while ip < len(tokens):
    opcode = tokens[ip]
    ip += 1

    if opcode.endswith(':'):
        out.write(f"\n; -- ETIQUETTE --\n")
        out.write(f"{opcode}\n")
    elif opcode == 'PUSH':
        value = tokens[ip]
        ip += 1
        out.write(f"\t; -- PUSH --\n")
        out.write(f"\tpush {value}\n")
    elif opcode == 'POP':
        out.write(f"; -- POP --\n")
        out.write(f"\tpop\n")
    elif opcode == 'ADD':
        out.write(f"\t; -- ADD --\n")
        # easiest way to add two numbers
        # out.write(f"\tpop rax\n")
        # out.write(f"\tpop rbx\n")
        # out.write(f"\tadd rax, rbx\n")
        # out.write(f"\tpush rax\n")

        # better way to add two numbers
        out.write(f"\tpop rax\n")
        out.write(f"\tadd qword [rsp], rax\n") # rsp is the top of the stack, so we can add directly to it (no need to pop) | the qword is to specify the size of the value to add | the [] is to dereference the pointer
    elif opcode == 'SUB':
        out.write(f"\t; -- SUB --\n")
        out.write(f"\tpop rax\n")
        out.write(f"\tsub qword [rsp], rax\n") # same as ADD but with sub
    elif opcode == "PRINT":
        string_literal_index = tokens[ip]
        ip += 1
        out.write(f"\t; -- PRINT --\n")
        # whith printf
        # out.write(f"\tlea rcx, string_literal_{string_literal_index}\n") # load effective address of the string literal into rcx | lea is used to load the address of a memory location into a register
        # out.write(f"\txor eax, eax\n")
        # out.write(f"\tcall printf\n")

        # with write syscall
        out.write(f"\tmov rax, 1\n") # 1 is the syscall number for write
        out.write(f"\tmov rdi, 1\n") # 1 is the file descriptor for stdout
        out.write(f"\tlea rsi, [string_literal_{string_literal_index}]\n") # load effective address of the string literal into rsi
        out.write(f"\tmov rdx, {len(string_literals[string_literal_index])}\n") # length of the string literal
        out.write(f"\tsyscall\n")
    elif opcode == 'READ':
        out.write(f"\t; -- READ --\n")
        # with scanf
        # out.write(f"\tlea rcx, read_format\n")
        # out.write(f"\tlea rdx, read_number\n")
        # out.write(f"\txor eax, eax\n")
        # out.write(f"\tcall scanf\n")
        # out.write(f"\tpush qword [read_number]\n")

        # with read syscall
        out.write(f"\tmov rax, 0\n") # 0 is the syscall number for read
        out.write(f"\tmov rdi, 0\n") # 0 is the file descriptor for stdin
        out.write(f"\tlea rsi, [input_buffer]\n") # load effective address of the input buffer into rsi
        out.write(f"\tmov rdx, 16\n") # length of the input buffer
        out.write(f"\tsyscall\n")

        out.write(f"\tcall convert_input_to_number\n") # call the function to convert the input buffer to a number
        out.write(f"\tpush rax\n") # push the result of the conversion

    elif opcode == 'JUMP.EQ.0':
        etiquette = tokens[ip]
        ip += 1
        out.write(f"\t; -- JUMP.EQ.0 --\n")
        out.write(f"\tcmp qword [rsp], 0\n")
        out.write(f"\tje {etiquette}\n")
    elif opcode == 'JUMP.GT.0':
        etiquette = tokens[ip]
        ip += 1
        out.write(f"\t; -- JUMP.GT.0 --\n")
        out.write(f"\tcmp qword [rsp], 0\n")
        out.write(f"\tjg {etiquette}\n")
    elif opcode == 'HALT':
        out.write(f"\t; -- HALT --\n")
        out.write(f"\tjmp _exit_etiquette\n")
    else:
        print(f"Unknown opcode: {opcode}")
        sys.exit(1)

out.write(f"""
; -- EXIT ETIQUETTE --
_exit_etiquette:
\tmov rax, 60 ; 60 is the syscall number for exit
\txor edi, edi ; 0 as return code
\tsyscall
""")

out.close()

# compile and link the assembly file

print(f"[CMD] Assembling")
os.system(f"nasm -f elf64 {asm_path}")

print(f"[CMD] Linking")
# os.system(f"gcc -o {asm_path.replace('.asm', '.out')} {asm_path.replace('.asm', '.o')}")
os.system(f"ld -o {asm_path.replace('.asm', '.out')} {asm_path.replace('.asm', '.o')}")

print(f"[CMD] Running")
os.system(f"./{asm_path.replace('.asm', '.out')}")
