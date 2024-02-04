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
        // inline std::map<std::string, unsigned int> getEtiquetteTracker() const { return etiquetteTracker; }

        void printTokens() const;

    protected:
    private:
        std::string _filePath;
        std::vector<std::any> _tokens;
        // unsigned int _tokenCounter;
        // std::map<std::string, unsigned int> etiquetteTracker;

        std::vector<std::string> _programLines;
};
