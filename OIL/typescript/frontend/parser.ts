import { Stmt, Program, Expr, BinaryExpr, NumericLiteral, Identifier, NullLiteral } from "./ast";
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
        return this.parseExpr();
    }

    private parseExpr(): Expr {
        return this.parseAdditiveExpr();
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
        let left = this.parsePrimaryExpr();

        while (this.at().value == "/" || this.at().value == "*" || this.at().value == "%") {
            const operator = this.eat().value;
            const right = this.parsePrimaryExpr();
            left = {
                kind: "BinaryExpr",
                left,
                right,
                operator
            } as BinaryExpr;
        }

        return left;
    }

    // Orders of prescidence
    // AdditiveExpr
    // MultiplicativeExpr
    // PrimaryExpr

    private parsePrimaryExpr(): Expr {
        const tk = this.at().type;

        switch (tk) {
            case TokenType.IDENTIFIER:
                return { kind: "Identifier", symbol: this.eat().value } as Identifier;
            case TokenType.NULL:
                this.eat(); // advance past null keywork
                return { kind: "NullLiteral", value: "null" } as NullLiteral;
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
                return {} as Expr;
        }
    }
}
