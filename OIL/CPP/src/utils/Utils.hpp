/*
** ZIPLEIX PROJECT, 2024
** OIL [WSL : Ubuntu]
** File description:
** Utils
*/

#pragma once

#include <vector>
#include <string>
#include <fstream>
#include <iostream>

class Utils {
    public:
        static std::vector<std::string> readFile(std::string file_path);
        static std::vector<std::string> split(const std::string &str, const std::string &delim);
        static std::string join(const std::vector<std::string> &tokens, const std::string &delim);
};
