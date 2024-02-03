/*
** ZIPLEIX PROJECT, 2024
** OIL [WSL : Ubuntu]
** File description:
** main
*/

#include <iostream>
#include <any>
#include <string>
#include <vector>
#include <fstream>
#include <map>
#include <sstream>
#include <functional>

#include "src/tokeniser/Tokeniser.hpp"
#include "src/interpreter/Interpreter.hpp"

int main(int argc, char const *argv[])
{
    if (argc != 2) {
        std::cout << "Usage: ./oil [file]" << std::endl;
        return 1;
    }

    std::string file_path = argv[1];

    Tokeniser tokeniser(file_path);
    tokeniser.tokenise();

    // tokeniser.printTokens();

    Interpreter interpreter(tokeniser.getTokens(), tokeniser.getEtiquetteTracker());
    interpreter.interprete();

    return 0;
}
