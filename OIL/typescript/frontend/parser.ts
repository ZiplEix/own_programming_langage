import { Stmt, Program, Expr, BinaryExpr, NumericLiteral, Identifier, VariableDeclaration, AssignmentExpr, Property, ObjectLiteral, CallExpr, MemberExpr, FunctionDeclaration } from "./ast";
import { tokenize, Token, TokenType } from "./lexer";

export default class Parser {
    private tokens: Token[] = [];

    private not_eof(): boolean {
        return this.tokens[0].type != TokenType.EOF;
    }

    private at() {
        return this.tokens[0] as Token;
    }

    private eat(): Token {
        const prev = this.tokens.shift();
        return prev as Token;
    }

    private expect(type: TokenType, err: any) {
        const prev = this.tokens.shift() as Token;
        if (!prev || prev.type != type) {
            console.error("parser error:\n", err, prev, " - Expecting: ", type);
            process.exit(1);
        }

        return prev;
    }

    public produceAST(sourceCode: string): Program {
        this.tokens = tokenize(sourceCode);
        const program: Program = {
            kind: "Program",
            body: []
        };

        // parse until EOF
        while (this.not_eof()) {
            program.body.push(this.parseStmt());
        }

        return program;
    }

    private parseStmt(): Stmt {
        // skip to parseExpr
        switch (this.at().type) {
            case TokenType.LET:
            case TokenType.CONST:
                return this.parseVariableDeclaration();

            case TokenType.FUNCTION:
                return this.parseFunctionDeclaration();

            default:
                return this.parseExpr();
        }
    }

    private parseFunctionDeclaration(): Stmt {
        this.eat(); // skip "function" keyword
        const name = this.expect(TokenType.IDENTIFIER, "Expected function name after 'function' keyword").value;

        const args = this.parseArgs();
        const params: string[] = [];
        for (const arg of args) {
            if (arg.kind !== "Identifier") {
                console.log(arg);
                throw "Expected identifier as function parameter";
            }
            params.push((arg as Identifier).symbol);
        }

        this.expect(TokenType.OPEN_BRACE, "Expected \"{\" after function declaration");

        const body: Stmt[] = []

        while (this.not_eof() && this.at().type !== TokenType.CLOSE_BRACE) {
            body.push(this.parseStmt());
        }

        this.expect(TokenType.CLOSE_BRACE, "Expected \"}\" after function declaration");

        const func = {
            kind: "FunctionDeclaration",
            name,
            params,
            body
        } as FunctionDeclaration;

        return func;
    }

    // LET IDENTIFIER;
    // ( LET | CONST ) IDENTIFIER = EXPR;
    parseVariableDeclaration(): Stmt {
        const isConst = this.eat().type == TokenType.CONST;
        const identifier = this.expect(TokenType.IDENTIFIER, "Expected identifier after let or const").value;

        if (this.at().type == TokenType.SEMI_COLUMN) {
            this.eat(); // skip ";"
            if (isConst) {
                throw "Const variable declaration must have a value";
            }

            return { kind: "variableDeclaration", constant: false, identifier } as VariableDeclaration;
        }

        this.expect(TokenType.EQUALS, "Expected \"=\" token after identifier in variable declaration");

        const declaration = { kind: "variableDeclaration", value: this.parseExpr(), constant: isConst, identifier } as VariableDeclaration;

        this.expect(TokenType.SEMI_COLUMN, "Expected \";\" after variable declaration");

        return declaration;
    }

    private parseExpr(): Expr {
        return this.parseAssignmentExpr();
    }

    private parseAssignmentExpr(): Expr {
        const left = this.parseObjectExpr();

        if (this.at().type == TokenType.EQUALS) {
            this.eat(); // skip "="
            const value = this.parseAssignmentExpr();
            return {
                kind: "AssignmentExpr",
                assigne: left,
                value: value
            } as AssignmentExpr;
        }

        return left;
    }

    private parseObjectExpr(): Expr {
        // { Props[] }
        if (this.at().type !== TokenType.OPEN_BRACE) {
            return this.parseAdditiveExpr();
        }

        this.eat(); // skip "{"
        const properties = new Array<Property>();

        while (this.not_eof() && this.at().type != TokenType.CLOSE_BRACE) {
            // { key: val, key2: val2 }
            const key = this.expect(TokenType.IDENTIFIER, "Expected identifier as key in object literal").value;

            // Allows shorthand property declaration: pair -> { key, }
            if (this.at().type == TokenType.COMMA) {
                this.eat(); // skip ","
                properties.push({ key, kind: "Property" } as Property);
                continue;
            } // Allows shorthand property declaration: pair -> { key }
            else if (this.at().type == TokenType.CLOSE_BRACE) {
                properties.push({ key, kind: "Property" } as Property);
                continue;
            }

            // { key: value }
            this.expect(TokenType.COLON, "Expected \":\" after key in object literal");
            const value = this.parseExpr();

            properties.push({ kind: "Property", key, value } as Property);
            if (this.at().type !== TokenType.CLOSE_BRACE) {
                this.expect(TokenType.COMMA, "Expected \",\" or \"]\" after property in object literal");
            }
        }

        this.expect(TokenType.CLOSE_BRACE, "Expected \"}\" after object literal");

        return { kind: "ObjectLiteral", properties } as ObjectLiteral;
    }

    private parseAdditiveExpr(): Expr {
        let left = this.parseMultiplicativeExpr();

        while (this.at().value == "+" || this.at().value == "-") {
            const operator = this.eat().value;
            const right = this.parseMultiplicativeExpr();
            left = {
                kind: "BinaryExpr",
                left,
                right,
                operator
            } as BinaryExpr;
        }

        return left;
    }

    private parseMultiplicativeExpr(): Expr {
        let left = this.parseCallMemberExpr();

        while (this.at().value == "/" || this.at().value == "*" || this.at().value == "%") {
            const operator = this.eat().value;
            const right = this.parseCallMemberExpr();
            left = {
                kind: "BinaryExpr",
                left,
                right,
                operator
            } as BinaryExpr;
        }

        return left;
    }

    // foo.x()
    private parseCallMemberExpr(): Expr {
        const member = this.parseMemberExpr();

        if (this.at().type == TokenType.OPEN_PAREN) {
            return this.parseCallExpr(member);
        }

        return member;
    }

    private parseCallExpr(caller: Expr): Expr {
        let callExpr: Expr = {
            kind: "CallExpr",
            caller,
            args: this.parseArgs()
        } as CallExpr;

        if (this.at().type == TokenType.OPEN_PAREN) {
            callExpr = this.parseCallExpr(callExpr);
        }

        return callExpr;
    }

    private parseArgs(): Expr[] {
        this.expect(TokenType.OPEN_PAREN, "Expected \"(\" before argument list");
        const args = this.at().type == TokenType.CLOSE_PAREN ? [] : this.parsArgsList();

        this.expect(TokenType.CLOSE_PAREN, "Expected \")\" after argument list");

        return args;
    }

    private parsArgsList(): Expr[] {
        const args = [this.parseAssignmentExpr()];

        while (this.at().type == TokenType.COMMA && this.eat()) {
            args.push(this.parseAssignmentExpr());
        }

        return args;
    }

    private parseMemberExpr(): Expr {
        let object = this.parsePrimaryExpr();

        while (this.at().type == TokenType.DOT || this.at().type == TokenType.OPEN_BRACKET) {
            const operator = this.eat();
            let property: Expr;
            let computed: boolean;

            // non-computed values aka: dot.expr
            if (operator.type == TokenType.DOT) {
                computed = false;
                // get identifier
                property = this.parsePrimaryExpr();

                if (property.kind != "Identifier") {
                    throw "Cannot use dot operator without right hand side being an identifier";
                }
            } else { // this allow obj[computedValue]
                computed = true;
                // get expression
                property = this.parseExpr();
                this.expect(TokenType.CLOSE_BRACKET, "Expected \"]\" after computed property");
            }

            object = { kind: "MemberExpr", object, property, computed } as MemberExpr;
        }

        return object;
    }

    // Orders of prescidence
    // Assignment
    // Object
    // AdditiveExpr
    // MultiplicativeExpr
    // call
    // Member
    // PrimaryExpr

    private parsePrimaryExpr(): Expr {
        const tk = this.at().type;

        switch (tk) {
            case TokenType.IDENTIFIER:
                return { kind: "Identifier", symbol: this.eat().value } as Identifier;
            case TokenType.NUMBER:
                return { kind: "NumericLiteral", value: parseFloat(this.eat().value) } as NumericLiteral;

            case TokenType.OPEN_PAREN:
                this.eat(); // skip "("
                const expr = this.parseExpr();
                this.expect(TokenType.CLOSE_PAREN, "Unexpected token found inside parenthesised expression. Expected \")\""); // skip ")"
                return expr;

            default:
                console.error("Unexpected token found during parsing !", this.at());
                process.exit(1);
        }
    }
}
