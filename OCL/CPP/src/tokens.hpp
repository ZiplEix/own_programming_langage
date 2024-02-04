/*
** ZIPLEIX PROJECT, 2024
** own_langages [WSL : Ubuntu]
** File description:
** tokens
*/

#pragma once

#include <string>
#include <optional>

typedef enum {
    HALT,
    VALUE,
    ETIQUETTE,
    PUSH,
    POP,
    ADD,
    SUB,
    PRINT,
    READ,
    JUMP_EQ_0,
    JUMP_GT_0
} Tokens;

typedef enum {
    INT,
    STRING_LITERAL
} ValueTypes;

// Parent class for all tokens
class Token
{
    public:
        Token(Tokens type) : _type(type) {};
        ~Token() = default;

        Tokens getType() const { return _type; }
        virtual void print() const = 0;
    protected:
        Tokens _type;
    private:
};

// Token for instruction
class TokenInstruction : public Token
{
    public:
        TokenInstruction(Tokens type) : Token(type) {};
        ~TokenInstruction() = default;

        void print() const override {
            switch (_type)
            {
            case Tokens::HALT:
                std::cout << "HALT";
                break;
            case Tokens::PUSH:
                std::cout << "PUSH";
                break;
            case Tokens::POP:
                std::cout << "POP";
                break;
            case Tokens::ADD:
                std::cout << "ADD";
                break;
            case Tokens::SUB:
                std::cout << "SUB";
                break;
            case Tokens::PRINT:
                std::cout << "PRINT";
                break;
            case Tokens::READ:
                std::cout << "READ";
                break;
            case Tokens::JUMP_EQ_0:
                std::cout << "JUMP.EQ.0";
                break;
            case Tokens::JUMP_GT_0:
                std::cout << "JUMP.GT.0";
                break;
            default:
                break;
            }
        }

    private:
};

// Token for value (int or stringLiteral)
class TokenValue : public Token
{
    public:
        TokenValue(int intValue) : Token(Tokens::VALUE), _valueType(ValueTypes::INT), _intValue(intValue) {};
        TokenValue(std::string stringValue) : Token(Tokens::VALUE), _valueType(ValueTypes::STRING_LITERAL),_stringValue(stringValue) {};
        ~TokenValue() = default;

        ValueTypes getValueType() const { return _valueType; }
        int getIntValue() const { return _intValue; }
        std::string getStringValue() const { return _stringValue; }

        void print() const override {
            if (_valueType == ValueTypes::INT) {
                std::cout << _intValue;
            } else {
                std::cout << _stringValue;
            }
        }

    private:
        ValueTypes _valueType;
        int _intValue;
        std::string _stringValue;
};

// Token for etiquette
class TokenEtiquette : public Token
{
    public:
        TokenEtiquette(std::string etiquette) : Token(Tokens::ETIQUETTE), _etiquette(etiquette) {};
        ~TokenEtiquette() = default;

        std::string getEtiquette() const { return _etiquette; }

        void print() const override {
            std::cout << _etiquette;
        }
    private:
        std::string _etiquette;
};
