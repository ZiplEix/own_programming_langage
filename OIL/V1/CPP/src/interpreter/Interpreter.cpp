/*
** ZIPLEIX PROJECT, 2024
** OIL [WSL : Ubuntu]
** File description:
** Interpreter
*/

#include "Interpreter.hpp"

Interpreter::Interpreter(std::vector<std::any> tokens, std::map<std::string, unsigned int> etiquetteTracker, size_t stackSize): _tokens(tokens), _etiquetteTracker(etiquetteTracker), _pc(0), _stack(stackSize)
{
    _opcodes["HALT"] = std::bind(&Interpreter::halt, this);
    _opcodes["PUSH"] = std::bind(&Interpreter::push, this);
    _opcodes["POP"] = std::bind(&Interpreter::pop, this);
    _opcodes["ADD"] = std::bind(&Interpreter::add, this);
    _opcodes["SUB"] = std::bind(&Interpreter::sub, this);
    _opcodes["PRINT"] = std::bind(&Interpreter::print, this);
    _opcodes["READ"] = std::bind(&Interpreter::read, this);
    _opcodes["JUMP.EQ.0"] = std::bind(&Interpreter::jumpEq0, this);
    _opcodes["JUMP.GT.0"] = std::bind(&Interpreter::jumpGt0, this);
}

void Interpreter::interprete() {
    if (_tokens.empty()) {
        std::cerr << "Error: No tokens to interprete" << std::endl;
        return;
    }

    while (1) {
        if (_pc >= _tokens.size()) {
            break;
        }
        std::string opcode = std::any_cast<std::string>(_tokens[_pc]);
        _pc++;

        if (_opcodes.find(opcode) == _opcodes.end()) {
            std::cerr << "Error: Unknown opcode " << opcode << std::endl;
            return;
        } else {
            if (_opcodes[opcode]()) {
                break;
            }
        }
    }
}

char Interpreter::halt() {
    return 1;
}

char Interpreter::push() {
    int value = std::any_cast<int>(_tokens[_pc]);
    _pc++;
    _stack.push(value);
    return 0;
}

char Interpreter::pop() {
    _stack.pop();
    return 0;
}

char Interpreter::add() {
    int a = _stack.top();
    _stack.pop();
    int b = _stack.top();
    _stack.pop();
    _stack.push(a + b);
    return 0;
}

char Interpreter::sub() {
    int a = _stack.top();
    _stack.pop();
    int b = _stack.top();
    _stack.pop();
    _stack.push(b - a);
    return 0;
}

char Interpreter::print() {
    std::string string_literal = std::any_cast<std::string>(_tokens[_pc]);
    _pc++;
    std::cout << string_literal << std::endl;
    return 0;
}

char Interpreter::read() {
    int value;
    std::cin >> value;
    _stack.push(value);
    return 0;
}

char Interpreter::jumpEq0() {
    std::string etiquette = std::any_cast<std::string>(_tokens[_pc]);
    if (_stack.top() == 0) {
        _pc = _etiquetteTracker[etiquette];
    } else {
        _pc++;
    }
    return 0;
}

char Interpreter::jumpGt0() {
    std::string etiquette = std::any_cast<std::string>(_tokens[_pc]);
    if (_stack.top() > 0) {
        _pc = _etiquetteTracker[etiquette];
    } else {
        _pc++;
    }
    return 0;
}
