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

#include "..//utils/Utils.hpp"

class Tokeniser {
    public:
        Tokeniser(std::string filePath);
        ~Tokeniser() = default;

        void tokenise();

        inline std::vector<std::any> getTokens() const { return _tokens; }

        void printTokens() const;

    protected:
    private:
        std::string _filePath;
        std::vector<std::any> _tokens;

        std::vector<std::string> _programLines;
};
