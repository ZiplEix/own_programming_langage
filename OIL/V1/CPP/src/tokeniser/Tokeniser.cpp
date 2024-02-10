/*
** ZIPLEIX PROJECT, 2024
** OIL [WSL : Ubuntu]
** File description:
** Tokeniser
*/

#include "Tokeniser.hpp"

Tokeniser::Tokeniser(std::string filePath) : _filePath(filePath), _tokenCounter(0)
{
    sciptLines = Utils::readFile(_filePath);
}

void Tokeniser::tokenise()
{
    for (auto line : sciptLines) {
        // check if line is empty
        if (line.empty()) {
            continue;
        }

        std::vector<std::string> lineTokens = Utils::split(line, " ");
        std::string opcode = lineTokens[0];

        // check if is a etiquette
        if (opcode.back() == ':') {
            std::string etiquette = opcode.substr(0, opcode.size() - 1);
            if (etiquetteTracker.find(etiquette) != etiquetteTracker.end()) {
                std::cerr << "Error: Etiquette " << etiquette << " already exists" << std::endl;
                return;
            }
            etiquetteTracker[etiquette] = _tokenCounter;
            continue;
        }

        // store opcode token
        _tokens.push_back(opcode);
        _tokenCounter++;

        // handle each opcode arguments
        if (opcode == "PUSH") {
            if (lineTokens.size() != 2) {
                std::cerr << "Error: PUSH opcode must have 1 argument" << std::endl;
                return;
            }
            int value = std::stoi(lineTokens[1]);
            _tokens.push_back(value);
            _tokenCounter++;
        } else if (opcode == "PRINT") {
            if (lineTokens.size() == 1) {
                std::cerr << "Error: PRINT opcode must have 0 argument" << std::endl;
                return;
            }
            // parse the string literal
            std::string string_literal = Utils::join(std::vector<std::string>(lineTokens.begin() + 1, lineTokens.end()), " ");
            _tokens.push_back(string_literal.substr(1, string_literal.size() - 2));
            _tokenCounter++;
        } else if (opcode == "JUMP.EQ.0") {
            if (lineTokens.size() != 2) {
                std::cerr << "Error: JUMP.EQ.0 opcode must have 1 argument" << std::endl;
                return;
            }
            std::string etiquette = lineTokens[1];
            _tokens.push_back(etiquette);
            _tokenCounter++;
        } else if (opcode == "JUMP.GT.0") {
            if (lineTokens.size() != 2) {
                std::cerr << "Error: JUMP.GT.0 opcode must have 1 argument" << std::endl;
                return;
            }
            std::string etiquette = lineTokens[1];
            _tokens.push_back(etiquette);
            _tokenCounter++;
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
