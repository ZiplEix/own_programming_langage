/*
** ZIPLEIX PROJECT, 2024
** OIL [WSL : Ubuntu]
** File description:
** main
*/

#include <string>

#include "src/tokeniser/Tokeniser.hpp"
#include "src/assembler/Assembler.hpp"

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

    Assembler assembler(tokeniser.getTokens(), file_path);
    assembler.assemble();

    return 0;
}
