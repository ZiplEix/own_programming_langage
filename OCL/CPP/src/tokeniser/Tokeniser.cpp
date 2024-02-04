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

        // store tokens
        // if the opcode end with a ':', it's an etiquette
        if (opcode.back() == ':') {
            std::string etiquette = opcode;
            _tokens.push_back(std::make_shared<TokenEtiquette>(etiquette));
            continue;
        } else if (opcode == "HALT") {
            _tokens.push_back(std::make_shared<TokenInstruction>(Tokens::HALT));
        } else if (opcode == "PUSH") {
            if (lineTokens.size() != 2) {
                std::cerr << "Error: PUSH opcode must have 1 argument" << std::endl;
                return;
            }
            _tokens.push_back(std::make_shared<TokenInstruction>(Tokens::PUSH));
            int value = std::stoi(lineTokens[1]);
            _tokens.push_back(std::make_shared<TokenValue>(value));
        } else if (opcode == "POP") {
            _tokens.push_back(std::make_shared<TokenInstruction>(Tokens::POP));
        } else if (opcode == "ADD") {
            _tokens.push_back(std::make_shared<TokenInstruction>(Tokens::ADD));
        } else if (opcode == "SUB") {
            _tokens.push_back(std::make_shared<TokenInstruction>(Tokens::SUB));
        } else if (opcode == "READ") {
            _tokens.push_back(std::make_shared<TokenInstruction>(Tokens::READ));
        } else if (opcode == "PRINT") {
            if (lineTokens.size() == 1) {
                std::cerr << "Error: PRINT opcode must have 0 argument" << std::endl;
                return;
            }
            _tokens.push_back(std::make_shared<TokenInstruction>(Tokens::PRINT));
            std::string string_literal = Utils::join(std::vector<std::string>(lineTokens.begin() + 1, lineTokens.end()), " ");
            _tokens.push_back(std::make_shared<TokenValue>(string_literal.substr(1, string_literal.size() - 2)));
        } else if (opcode == "JUMP.EQ.0") {
            if (lineTokens.size() != 2) {
                std::cerr << "Error: JUMP.EQ.0 opcode must have 1 argument" << std::endl;
                return;
            }
            _tokens.push_back(std::make_shared<TokenInstruction>(Tokens::JUMP_EQ_0));
            std::string etiquette = lineTokens[1];
            _tokens.push_back(std::make_shared<TokenEtiquette>(etiquette));
        } else if (opcode == "JUMP.GT.0") {
            if (lineTokens.size() != 2) {
                std::cerr << "Error: JUMP.GT.0 opcode must have 1 argument" << std::endl;
                return;
            }
            _tokens.push_back(std::make_shared<TokenInstruction>(Tokens::JUMP_GT_0));
            std::string etiquette = lineTokens[1];
            _tokens.push_back(std::make_shared<TokenEtiquette>(etiquette));
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

        (*it)->print();
    }
    std::cout << "']" << std::endl;
}
