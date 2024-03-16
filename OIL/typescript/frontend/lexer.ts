// let x = 45 + ( foo * bat )
// [ LetToken, IdentifierTk, EqualToken, NumberToken]

export enum TokenType {
    // Lieral Types
    NUMBER,
    IDENTIFIER,

    // Punctuation
    SEMI_COLUMN, // ;
    EQUALS, // =
    COMMA, // ,
    DOT, // .
    COLON, // :
    OPEN_PAREN, // (
    CLOSE_PAREN, // )
    OPEN_BRACE, // {
    CLOSE_BRACE, // }
    OPEN_BRACKET, // [
    CLOSE_BRACKET, // ]
    BINARY_OPERATOR, // +, -, *, /, %

    // Keywords
    LET,
    CONST,
    FUNCTION,

    // End of file
    EOF,
}

const KEYWORDS: Record<string, TokenType> = {
    "let": TokenType.LET,
    "const": TokenType.CONST,
    "function": TokenType.FUNCTION,
};

export interface Token {
    value: string,
    type: TokenType
}

function token(value = "", type: TokenType): Token {
    return { value, type };
}

function isAlpha(str: string): boolean {
    return str.toUpperCase() != str.toLowerCase();
}

function isInt(str: string): boolean {
    const c = str.charCodeAt(0);
    const bounds = ['0'.charCodeAt(0), '9'.charCodeAt(0)];
    return c >= bounds[0] && c <= bounds[1];
}

function isWhitespace(str: string): boolean {
    return str == ' ' || str == '\n' || str == '\t' || str == '\r';
}

export function tokenize(sourceCode: string): Token[] {
    const tokens = new Array<Token>();
    const src = sourceCode.split('');

    // build each token until end of file
    while (src.length > 0) {
        // ONE CHARACTER TOKENS
        if (src[0] == '(') {
            tokens.push(token(src.shift(), TokenType.OPEN_PAREN))
        }
        else if (src[0] == ')') {
            tokens.push(token(src.shift(), TokenType.CLOSE_PAREN))
        }
        else if (src[0] == '{') {
            tokens.push(token(src.shift(), TokenType.OPEN_BRACE))
        }
        else if (src[0] == '}') {
            tokens.push(token(src.shift(), TokenType.CLOSE_BRACE))
        }
        else if (src[0] == '[') {
            tokens.push(token(src.shift(), TokenType.OPEN_BRACKET))
        }
        else if (src[0] == ']') {
            tokens.push(token(src.shift(), TokenType.CLOSE_BRACKET))
        }
        // HANDLE BINARIES OPERATORS
        else if (src[0] == '+' || src[0] == '-' || src[0] == '*' || src[0] == '/' || src[0] == '%') {
            tokens.push(token(src.shift(), TokenType.BINARY_OPERATOR))
        }
        // HANDLE CONDITIONAL AND ASSIGNMENT TOKENS
        else if (src[0] == '=') {
            tokens.push(token(src.shift(), TokenType.EQUALS))
        }
        else if (src[0] == ';') {
            tokens.push(token(src.shift(), TokenType.SEMI_COLUMN))
        }
        else if (src[0] == ':') {
            tokens.push(token(src.shift(), TokenType.COLON))
        }
        else if (src[0] == ',') {
            tokens.push(token(src.shift(), TokenType.COMMA))
        }
        else if (src[0] == '.') {
            tokens.push(token(src.shift(), TokenType.DOT))
        }
        // HANDLE MULTI-CHARACTER KEYWORDS, TOKENS, IDENTIFIERS, ETC...
        else {
            // Handle multi-character tokens

            // build number token
            if (isInt(src[0])) {
                let num = "";
                while (src.length > 0 && isInt(src[0])) {
                    num += src.shift();
                }
                tokens.push(token(num, TokenType.NUMBER));
            } else if (isAlpha(src[0])) {
                let identifier = "";
                while (src.length > 0 && isAlpha(src[0])) {
                    identifier += src.shift();
                }
                // check for reserved keywords
                const reserved = KEYWORDS[identifier];
                if (typeof reserved == "number") {
                    tokens.push(token(identifier, reserved));
                } else {
                    tokens.push(token(identifier, TokenType.IDENTIFIER));
                }
            } else if (isWhitespace(src[0])) {
                src.shift();
            } else {
                console.log("Unreconized character: \"" + src[0] + "\"");
                // quit the programme
                process.exit(1);
            }
        }
    }

    tokens.push(token("EOF", TokenType.EOF));

    return tokens;
}
