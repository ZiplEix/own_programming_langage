/*
** ZIPLEIX PROJECT, 2024
** own_langages [WSL : Ubuntu]
** File description:
** Assembler
*/

#pragma once

#include <iostream>
#include <any>
#include <string>
#include <vector>
#include <map>
#include <functional>
#include <fstream>
#include <memory>

#include "../tokens.hpp"

class Assembler {
    public:
        Assembler(std::vector<std::shared_ptr<Token>> tokens, std::string file_path);
        ~Assembler();

        void assemble();

    protected:
    private:
        std::string _file_path;
        std::string _asmPath;
        std::vector<std::shared_ptr<Token>> _tokens;
        unsigned int _pc;
        std::vector<std::string> _stringLiteral;

        std::ofstream _asmFile;

        std::map<std::string, std::function<char()>> _opcodes;

        Token *getNextToken();

        void getStringLiteral();
        void writeHeader();
        void writeBss();
        void writeData();
        void writeDefaultText();
        void writeCustomText();
};
