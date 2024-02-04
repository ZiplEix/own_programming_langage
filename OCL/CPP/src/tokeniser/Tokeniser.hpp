/*
** ZIPLEIX PROJECT, 2024
** OIL [WSL : Ubuntu]
** File description:
** Tokeniser
*/

#pragma once

#include <iostream>
#include <any>
#include <string>
#include <vector>
#include <map>
#include <memory>

#include "../tokens.hpp"
#include "../utils/Utils.hpp"

class Tokeniser {
    public:
        Tokeniser(std::string filePath);
        ~Tokeniser() = default;

        void tokenise();

        inline std::vector<std::shared_ptr<Token>> getTokens() const { return _tokens; }

        void printTokens() const;

    protected:
    private:
        std::string _filePath;
        std::vector<std::shared_ptr<Token>> _tokens;

        std::vector<std::string> _programLines;
};
