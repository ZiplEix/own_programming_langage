import sys

# read the name of the script file
script_path = sys.argv[1]

#############
# TOKENISER #
#############

# read line one by one
script_lines = []
with open(script_path, 'r') as script_file:
    script_lines = [line.strip() for line in script_file.readlines()]

tokens = []
token_counter = 0
etiquette_tracker = {}
for line in script_lines:
    parts = line.split(' ')
    opcode = parts[0]

    # check empty line
    if len(opcode) == 0:
        continue

    # check if it is an etiquette
    if opcode[-1] == ':':
        etiquette_tracker[opcode[:-1]] = token_counter
        continue

    # store opcode token
    tokens.append(opcode)
    token_counter += 1

    # handle each opcode
    if opcode == 'PUSH':
        # need a number after PUSH
        number = int(parts[1])
        tokens.append(number)
        token_counter += 1
    elif opcode == 'PRINT':
        # parse string literal
        string_literal = ' '.join(parts[1:])[1:-1]
        tokens.append(string_literal)
        token_counter += 1
    elif opcode == 'JUMP.EQ.0':
        etiquette = parts[1]
        tokens.append(etiquette)
        token_counter += 1
    elif opcode == 'JUMP.GT.0':
        etiquette = parts[1]
        tokens.append(etiquette)
        token_counter += 1

###############
# INTERPRETER #
###############

class Stack:
    def __init__(self, size: int):
        self.buf = [0 for _ in range(size)]
        self.sp = -1

    def push(self, value: int):
        self.sp += 1
        self.buf[self.sp] = value

    def pop(self) -> int:
        value = self.buf[self.sp]
        self.sp -= 1
        return value

    def top(self) -> int:
        return self.buf[self.sp]

    def print(self):
        print(self.buf[:self.sp + 1])

pc = 0
stack = Stack(256)

while tokens[pc] != 'HALT':
    opcode = tokens[pc]
    pc += 1

    if opcode == 'PUSH':
        value = tokens[pc]
        pc += 1
        stack.push(value)
    elif opcode == 'POP':
        stack.pop()
    elif opcode == 'ADD':
        value1 = stack.pop()
        value2 = stack.pop()
        stack.push(value1 + value2)
    elif opcode == 'SUB':
        value1 = stack.pop()
        value2 = stack.pop()
        stack.push(value2 - value1)
    elif opcode == 'PRINT':
        value = tokens[pc]
        pc += 1
        print(value)
    elif opcode == 'READ':
        value = int(input())
        stack.push(value)
    elif opcode == 'JUMP.EQ.0':
        number = stack.top()
        if number == 0:
            pc = etiquette_tracker[tokens[pc]]
        else:
            pc += 1
    elif opcode == 'JUMP.GT.0':
        number = stack.top()
        if number > 0:
            pc = etiquette_tracker[tokens[pc]]
        else:
            pc += 1
    else:
        raise Exception(f'Unknown opcode: {opcode}')

