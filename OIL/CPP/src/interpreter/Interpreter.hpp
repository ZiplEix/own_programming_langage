/*
** ZIPLEIX PROJECT, 2024
** OIL [WSL : Ubuntu]
** File description:
** Interpreter
*/

#pragma once

#include <iostream>
#include <any>
#include <string>
#include <vector>
#include <map>
#include <functional>

#include "LimitedStack.hpp"

class Interpreter {
    public:
        Interpreter(std::vector<std::any> tokens, std::map<std::string, unsigned int> etiquetteTracker, size_t stackSize = 256);
        ~Interpreter() = default;

        void interprete();

    protected:
    private:
        std::vector<std::any> _tokens;
        unsigned int _pc;
        LimitedStack<int> _stack;
        std::map<std::string, unsigned int> _etiquetteTracker;

        std::map<std::string, std::function<char()>> _opcodes;

        char halt();
        char push();
        char pop();
        char add();
        char sub();
        char print();
        char read();
        char jumpEq0();
        char jumpGt0();
};
