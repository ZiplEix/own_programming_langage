/*
** ZIPLEIX PROJECT, 2024
** OIL [WSL : Ubuntu]
** File description:
** Utils
*/

#include "Utils.hpp"

std::vector<std::string> Utils::readFile(std::string file_path)
{
    std::vector<std::string> lines;
    std::string line;
    std::ifstream file(file_path);

    if (file.is_open()) {
        while (std::getline(file, line)) {
            lines.push_back(line);
        }
        file.close();
    } else {
        std::cerr << "Unable to open file " << file_path << std::endl;
    }
    return lines;
}

std::vector<std::string> Utils::split(const std::string &str, const std::string &delim)
{
    std::vector<std::string> tokens;
    size_t prev = 0, pos = 0;
    do {
        pos = str.find(delim, prev);
        if (pos == std::string::npos)
            pos = str.length();
        std::string token = str.substr(prev, pos-prev);
        if (!token.empty())
            tokens.push_back(token);
        prev = pos + delim.length();
    } while (pos < str.length() && prev < str.length());
    return tokens;
}

std::string Utils::join(const std::vector<std::string> &tokens, const std::string &delim)
{
    std::string result;
    for (size_t i = 0; i < tokens.size(); i++) {
        result += tokens[i];
        if (i != tokens.size() - 1) {
            result += delim;
        }
    }
    return result;
}