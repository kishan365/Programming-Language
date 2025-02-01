
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define MAX_IDENTIFIER_SIZE 128

enum TokenKind {
    TokenKind_Invalid,
    TokenKind_Number,
    TokenKind_Identifier,
    TokenKind_Equal,      // =
    TokenKind_Semicolon,  // ;
    TokenKind_Plus,       // +
    TokenKind_Minus,      // -
    TokenKind_Multiply,   // *
    TokenKind_Divide,     // /
    TokenKind_OpenBrace,  // (
    TokenKind_CloseBrace, // )
    TokenKind_BitwiseOr,  // |
    TokenKind_BitwiseAnd, // &
    TokenKind_BitwiseNot, // ~
    TokenKind_EqualEqual, // ==
    TokenKind_Or,         // ||
    TokenKind_And,        // &&
};

const char *TokenNames[] = {
    "invalid", "number", "identifier", "=", ";", "+", "-", "*", "/", "(", ")", "|", "&", "~", "==", "||", "&&"
};

struct Token {
    TokenKind Kind;
    char      Identifier[MAX_IDENTIFIER_SIZE];
    int       IdentifierLen;
    double    Number;
};

struct Tokenizer {
    const char *Input;
    int         Position;
    int         Length;
};

bool SkipWhitespaces(Tokenizer *tokenizer) {
    char ch = tokenizer->Input[tokenizer->Position];
    if (!isspace(ch)) return false;

    for (; tokenizer->Position < tokenizer->Length; ++tokenizer->Position) {
        ch = tokenizer->Input[tokenizer->Position];
        if (!isspace(ch)) break;
    }

    return true;
}

const char      SingleCharTokenInput[] = { '=', ';', '+', '-', '*', '/', '(', ')', '|', '&', '~' };
const TokenKind SingleCharTokenOutput[] = { 
    TokenKind_Equal, 
    TokenKind_Semicolon, 
    TokenKind_Plus, 
    TokenKind_Minus,
    TokenKind_Multiply,
    TokenKind_Divide,
    TokenKind_OpenBrace,
    TokenKind_CloseBrace,
    TokenKind_BitwiseOr,
    TokenKind_BitwiseAnd,
    TokenKind_BitwiseNot,
};

struct DoubleCharToken {
    char A, B;
};

const DoubleCharToken DoubleCharTokenInput[] = { {'=', '='}, { '|', '|'}, { '&', '&' } };
const TokenKind       DoubleCharTokenOutput[] = {
    TokenKind_EqualEqual, TokenKind_Or, TokenKind_And
};

#define ArrayCount(a) (sizeof(a)/sizeof((a)[0]))

bool IsNum(char c) {
    return c >= '0' && c <= '9';
}

bool IsNumberSeparator(char c) {
    for (int iter = 0; iter < ArrayCount(SingleCharTokenInput); ++iter) {
        if (c == SingleCharTokenInput[iter])
            return true;
    }
    for (int iter = 0; iter < ArrayCount(DoubleCharTokenInput); ++iter) {
        if (c == DoubleCharTokenInput[iter].A)
            return true;
    }
    return isspace(c);
}

bool Tokenize(Tokenizer *tokenizer, Token *token) {
    memset(token, 0, sizeof(*token));

    while (tokenizer->Position < tokenizer->Length) {
        if (!SkipWhitespaces(tokenizer))
            break;
    }

    if (tokenizer->Position >= tokenizer->Length)
        return false;

    char a = tokenizer->Input[tokenizer->Position];

    if (tokenizer->Position + 1 < tokenizer->Length) {
        char b = tokenizer->Input[tokenizer->Position + 1];
        for (int iter = 0; iter < ArrayCount(DoubleCharTokenInput); ++iter) {
            if (a == DoubleCharTokenInput[iter].A && b == DoubleCharTokenInput[iter].B) {
                token->Kind = DoubleCharTokenOutput[iter];
                tokenizer->Position += 2;
                return true;
            }
        }
    }

    for (int iter = 0; iter < ArrayCount(SingleCharTokenInput); ++iter) {
        if (a == SingleCharTokenInput[iter]) {
            token->Kind = SingleCharTokenOutput[iter];
            tokenizer->Position += 1;
            return true;
        }
    }

    if (IsNum(a)) {
        char str[MAX_IDENTIFIER_SIZE] = {};

        int len = 0;

        while (tokenizer->Position < tokenizer->Length) {
            a = tokenizer->Input[tokenizer->Position];
            if (IsNumberSeparator(a)) break;
            str[len] = a;
            len++;
            tokenizer->Position++;

            if (len == MAX_IDENTIFIER_SIZE) {
                printf("too long number\n");
                exit(1);
            }
        }

        char *endptr = 0;
        double number = strtod(str, &endptr);

        if (str + len != endptr) {
            printf("invalid number\n");
            exit(1);
        }

        token->Kind = TokenKind_Number;
        token->Number = number;

        return true;
    }

    if (isalnum(a) || a == '_') {
        token->Kind = TokenKind_Identifier;

        while (tokenizer->Position < tokenizer->Length) {
            a = tokenizer->Input[tokenizer->Position];
            if (!isalnum(a) && a != '_') {
                break;
            }
            token->Identifier[token->IdentifierLen] = a;
            token->IdentifierLen += 1;
            tokenizer->Position++;

            if (token->IdentifierLen == MAX_IDENTIFIER_SIZE) {
                printf("too long identifier\n");
                exit(1);
            }
        }

        return true;
    }

    printf("invalid character: %c\n", a);
    exit(1);

    return false;
}

void PrintToken(const Token *token) {
    printf("%s ", TokenNames[token->Kind]);

    if (token->Kind == TokenKind_Number) {
        printf("(%f)", token->Number);
    } else if (token->Kind == TokenKind_Identifier) {
        printf("(%s)", token->Identifier);
    }

    printf("\n");
}

int main() {
    const char *input = " xyz = y_  2";

    Tokenizer t;
    t.Input = input;
    t.Position = 0;
    t.Length = strlen(input);

    Token token;
    while (Tokenize(&t, &token)) {
        PrintToken(&token);
    }

    return 0;
}
