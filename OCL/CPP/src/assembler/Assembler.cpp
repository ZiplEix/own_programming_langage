/*
** EPITECH PROJECT, 2024
** own_langages [WSL : Ubuntu]
** File description:
** Assembler
*/

#include "Assembler.hpp"

Assembler::Assembler(std::vector<std::shared_ptr<Token>> tokens, std::string file_path): _file_path(file_path), _tokens(tokens), _pc(0)
{
    _asmPath = _file_path.substr(0, _file_path.find_last_of('.')) + ".asm";

    // open the asm file
    _asmFile.open(_asmPath, std::ios::out);
}

Assembler::~Assembler()
{
    _asmFile << "; -- EXIT ETIQUETTE --" << std::endl;
    _asmFile << "_exit_etiquette:" << std::endl;
    _asmFile << "\tmov rax, 60 ; 60 is the syscall number for exit" << std::endl;
    _asmFile << "\txor edi, edi ; 0 as return code" << std::endl;
    _asmFile << "\tsyscall" << std::endl << std::endl;

    _asmFile.close();
}

void Assembler::assemble()
{
    if (_tokens.empty()) {
        std::cerr << "Error: No tokens to assemble" << std::endl;
        return;
    }

    getStringLiteral();
    writeHeader();
    writeBss();
    writeData();
    writeDefaultText();
    writeCustomText();
}

Token *Assembler::getNextToken()
{
    if (_pc >= _tokens.size()) {
        return nullptr;
    }
    Token *token = _tokens[_pc].get();
    _pc++;
    return token;
}

void Assembler::getStringLiteral()
{
    for (size_t i = 0; i < _tokens.size(); i++) {
        if (_tokens[i]->getType() == Tokens::PRINT) {
            TokenValue *tokenValue = dynamic_cast<TokenValue *>(_tokens[i + 1].get());
            std::string stringLiteral = tokenValue->getStringValue();
            _tokens[i + 1] = std::make_shared<TokenValue>(_stringLiteral.size());
            _stringLiteral.push_back(stringLiteral);
        }
    }
}

void Assembler::writeHeader()
{
    _asmFile << "; -- header --" << std::endl;
    _asmFile << "bits 64" << std::endl;
    _asmFile << "default rel" << std::endl << std::endl;
}

void Assembler::writeBss()
{
    _asmFile << "; -- bss --" << std::endl;
    _asmFile << "section .bss" << std::endl;
    _asmFile << "input_buffer resb 16 ; reserve 16 bytes for the input buffer" << std::endl << std::endl;
}

void Assembler::writeData()
{
    _asmFile << "; -- data/constant section --" << std::endl;
    _asmFile << "section .data" << std::endl;
    for (size_t i = 0; i < _stringLiteral.size(); i++) {
        // 0 is the null terminator | db indicates that we are writing a byte
        _asmFile << "string_literal_" << i << " db \"" << _stringLiteral[i] << "\", 0" << std::endl;
    }
    _asmFile << std::endl;
}

void Assembler::writeDefaultText()
{
    _asmFile << "; -- Entry Point --" << std::endl;
    _asmFile << "section .text" << std::endl;
    _asmFile << "global _start" << std::endl << std::endl;

    // convert number function
    _asmFile << "; -- Function to convert input to number --" << std::endl;
    _asmFile << "; Assumes input is in 'input_buffer' | Result will be in RAX" << std::endl;
    _asmFile << "convert_input_to_number:" << std::endl;
    _asmFile << "    mov rsi, input_buffer  ; Input string address" << std::endl;
    _asmFile << "    xor rax, rax           ; Clear RAX for result" << std::endl;
    _asmFile << "convert_loop:" << std::endl;
    _asmFile << "    movzx rcx, byte [rsi]  ; Load next byte" << std::endl;
    _asmFile << "    test rcx, rcx          ; Check for null terminator" << std::endl;
    _asmFile << "    jz convert_done" << std::endl;
    _asmFile << "    sub rcx, '0'           ; Convert from ASCII to integer" << std::endl;
    _asmFile << "    imul rax, rax, 10      ; Multiply current result by 10" << std::endl;
    _asmFile << "    add rax, rcx           ; Add new digit" << std::endl;
    _asmFile << "    inc rsi                ; Move to next character" << std::endl;
    _asmFile << "    jmp convert_loop" << std::endl;
    _asmFile << "convert_done:" << std::endl;
    _asmFile << "    ret" << std::endl << std::endl;
}

void Assembler::writeCustomText()
{
    // starting point and shadow space
    _asmFile << "; -- Starting Point --" << std::endl;
    _asmFile << "_start:" << std::endl;
    _asmFile << "; shadow space (microsoft calling convention) (min 32 bytes)" << std::endl;
    _asmFile << "\tpush rbp" << std::endl;
    _asmFile << "\tmov rbp, rsp" << std::endl;
    _asmFile << "\tsub rsp, 32" << std::endl << std::endl;
    _asmFile << "; -- Program start --" << std::endl;

    // for (size_t i = 0; i < _tokens.size(); i++) {
    //     Tokens tokenType = _tokens[i]->getType();

    //     if (tokenType == Tokens::ETIQUETTE) {
    //         _asmFile << std::endl << "; -- ETIQUETTE --" << std::endl;
    //         TokenEtiquette *tokenEtiquette = dynamic_cast<TokenEtiquette *>(_tokens[i].get());
    //         _asmFile << tokenEtiquette->getEtiquette() << std::endl;
    //     } else if (tokenType == Tokens::PUSH) {
    //         i++;
    //         int value = dynamic_cast<TokenValue *>(_tokens[i + 1].get())->getIntValue();
    //     }
    // }

    while (1) {
        Token *token = getNextToken();
        if (token == nullptr) {
            break;
        }

        Tokens tokenType = token->getType();

        if (tokenType == Tokens::ETIQUETTE) {
            _asmFile << std::endl << "; -- ETIQUETTE --" << std::endl;
            TokenEtiquette *tokenEtiquette = dynamic_cast<TokenEtiquette *>(token);
            _asmFile << tokenEtiquette->getEtiquette() << std::endl;
        } else if (tokenType == Tokens::PUSH) {
            int value = dynamic_cast<TokenValue *>(getNextToken())->getIntValue();
            _asmFile << "\t; -- PUSH --" << std::endl;
            _asmFile << "\tpush " << value << std::endl;
        } else if (tokenType == Tokens::POP) {
            _asmFile << "\t; -- POP --" << std::endl;
            _asmFile << "\tpop" << std::endl;
        } else if (tokenType == Tokens::ADD) {
            _asmFile << "\t; -- ADD --" << std::endl;
            _asmFile << "\tpop rax" << std::endl;
            // rsp is the top of the stack, so we can add directly to it (no need to pop) | the qword is to specify the size of the value to add | the [] is to dereference the pointer
            _asmFile << "\tadd qword [rsp], rax" << std::endl;
        } else if (tokenType == Tokens::SUB) {
            _asmFile << "\t; -- SUB --" << std::endl;
            _asmFile << "\tpop rax" << std::endl;
            _asmFile << "\tsub qword [rsp], rax" << std::endl;
        } else if (tokenType == Tokens::PRINT) {
            _asmFile << "\t; -- PRINT --" << std::endl;
            int stringLiteralIndex = dynamic_cast<TokenValue *>(getNextToken())->getIntValue();
            _asmFile << "\tmov rax, 1" << std::endl; // 1 is the syscall number for write
            _asmFile << "\tmov rdi, 1" << std::endl; // 1 is the file descriptor for stdout
            // load the effectice adress of the string literal into rsi
            _asmFile << "\tlea rsi, [string_literal_" << stringLiteralIndex << "]" << std::endl;
            _asmFile << "\tmov rdx, " << _stringLiteral[stringLiteralIndex].size() << std::endl; // length of the string
            _asmFile << "\tsyscall" << std::endl;
        } else if (tokenType == Tokens::READ) {
            _asmFile << "\t; -- READ --" << std::endl;
            _asmFile << "\tmov rax, 0" << std::endl; // 0 is the syscall number for read
            _asmFile << "\tmov rdi, 0" << std::endl; // 0 is the file descriptor for stdin
            _asmFile << "\tlea rsi, [input_buffer]" << std::endl; // load the effective address of the input buffer into rsi
            _asmFile << "\tmov rdx, 16" << std::endl; // length of the input buffer
            _asmFile << "\tsyscall" << std::endl;

            _asmFile << "\tcall convert_input_to_number" << std::endl;
            _asmFile << "\tpush rax" << std::endl; // push the result onto the stack
        } else if (tokenType == Tokens::JUMP_EQ_0) {
            _asmFile << "\t; -- JUMP_EQ_0 --" << std::endl;
            _asmFile << "\tcmp qword [rsp], 0" << std::endl;
            _asmFile << "\tje " << dynamic_cast<TokenEtiquette *>(getNextToken())->getEtiquette() << std::endl;
        } else if (tokenType == Tokens::JUMP_GT_0) {
            _asmFile << "\t; -- JUMP_GT_0 --" << std::endl;
            _asmFile << "\tcmp qword [rsp], 0" << std::endl;
            _asmFile << "\tjg " << dynamic_cast<TokenEtiquette *>(getNextToken())->getEtiquette() << std::endl;
        } else if (tokenType == Tokens::HALT) {
            _asmFile << "\t; -- HALT --" << std::endl;
            _asmFile << "\tjmp _exit_etiquette" << std::endl;
        } else {
            std::cerr << "Error: Unknown token type: '";
            token->print();
            std::cout << "'" << std::endl;
            exit(1);
        }
    }

    _asmFile << "; -- Program end --" << std::endl << std::endl;
}
