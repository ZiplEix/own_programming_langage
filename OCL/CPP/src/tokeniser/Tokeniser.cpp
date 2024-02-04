/*
** ZIPLEIX PROJECT, 2024
** OIL [WSL : Ubuntu]
** File description:
** Tokeniser
*/

#include "Tokeniser.hpp"

Tokeniser::Tokeniser(std::string filePath) : _filePath(filePath)
{
    _programLines = Utils::readFile(_filePath);
}

void Tokeniser::tokenise()
{
    for (auto line : _programLines) {
        // check if line is empty
        if (line.empty()) {
            continue;
        }

        std::vector<std::string> lineTokens = Utils::split(line, " ");
        std::string opcode = lineTokens[0];

        // store opcode token
        _tokens.push_back(opcode);

        // store other tokens
        if (opcode == "PUSH") {
            if (lineTokens.size() != 2) {
                std::cerr << "Error: PUSH opcode must have 1 argument" << std::endl;
                return;
            }
            int value = std::stoi(lineTokens[1]);
            _tokens.push_back(value);
        } else if (opcode == "PRINT") {
            if (lineTokens.size() == 1) {
                std::cerr << "Error: PRINT opcode must have 0 argument" << std::endl;
                return;
            }
            // parse the string literal
            std::string string_literal = Utils::join(std::vector<std::string>(lineTokens.begin() + 1, lineTokens.end()), " ");
            _tokens.push_back(string_literal.substr(1, string_literal.size() - 2));
        } else if (opcode == "JUMP.EQ.0") {
            if (lineTokens.size() != 2) {
                std::cerr << "Error: JUMP.EQ.0 opcode must have 1 argument" << std::endl;
                return;
            }
            std::string etiquette = lineTokens[1];
            _tokens.push_back(etiquette);
        } else if (opcode == "JUMP.GT.0") {
            if (lineTokens.size() != 2) {
                std::cerr << "Error: JUMP.GT.0 opcode must have 1 argument" << std::endl;
                return;
            }
            std::string etiquette = lineTokens[1];
            _tokens.push_back(etiquette);
        }
    }
}

void Tokeniser::printTokens() const
{
    std::cout << "Tokens : ['";
    for (auto it = _tokens.begin(); it != _tokens.end(); ++it) {
        if (it != _tokens.begin()) {
            std::cout << "', '";
        }

        if (it->type() == typeid(std::string)) {
            std::cout << std::any_cast<std::string>(*it);
        } else if (it->type() == typeid(int)) {
            std::cout << std::any_cast<int>(*it);
        }
    }
    std::cout << "']" << std::endl;
}
