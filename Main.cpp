
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
    printf("    %s ", TokenNames[token->Kind]);

    if (token->Kind == TokenKind_Number) {
        printf("(%f)", token->Number);
    } else if (token->Kind == TokenKind_Identifier) {
        printf("(%s)", token->Identifier);
    }

    printf("\n");
}

enum BinaryOperatorKind {
    BinaryOperator_Plus,
    BinaryOperator_Minus,
    BinaryOperator_Multiply,
    BinaryOperator_Divide,
};

const TokenKind BinaryOperatorInput[] = {
    TokenKind_Plus,
    TokenKind_Minus,
    TokenKind_Multiply,
    TokenKind_Divide,
};

const BinaryOperatorKind BinaryOperatorOutput[] = {
    BinaryOperator_Plus,
    BinaryOperator_Minus,
    BinaryOperator_Multiply,
    BinaryOperator_Divide,
};

enum UnaryOperatorKind {
    UnaryOperator_Plus,
    UnaryOperator_Minus,
    UnaryOperator_BitwiseNot,
};

const TokenKind UnaryOperatorStartInput[] = {
    TokenKind_Plus,
    TokenKind_Minus,
};

const UnaryOperatorKind UnaryOperatorStartOutput[] = {
    UnaryOperator_Plus,
    UnaryOperator_Minus,
};

const TokenKind UnaryOperatorInput[] = {
    TokenKind_BitwiseNot,
};

const UnaryOperatorKind UnaryOperatorOutput[] = {
    UnaryOperator_BitwiseNot,
};

enum ExprKind {
    ExprKind_BinaryOperator,
    ExprKind_UnaryOperator,
    ExprKind_Assignment,
    ExprKind_Number,
    ExprKind_Identifier,
};

struct ExprNode {
    ExprKind Kind;
    ExprNode *Left;
    ExprNode *Right;
    BinaryOperatorKind BinaryOperator;
    UnaryOperatorKind UnaryOperator;
    Token SrcToken;
};

static ExprNode ExprNodeBuffers[8192];
static int ExprNodePos = 0;

ExprNode *ExprNodeCreate(ExprKind kind) {
    if (ExprNodePos == ArrayCount(ExprNodeBuffers)) {
        printf("error: out of memory");
        exit(1);
    }

    ExprNode *node = &ExprNodeBuffers[ExprNodePos];
    ExprNodePos++;
    memset(node, 0, sizeof(*node));
    node->Kind = kind;
    return node;
}

void ExprNodeReset() {
    ExprNodePos = 0;
}

ExprNode *ParseSubexpression(Tokenizer *t, bool start) {
    Token token;

    if (!Tokenize(t, &token)) {
        printf("error: expected operand\n");
        exit(1);
    }

    if (token.Kind == TokenKind_Number) {
        ExprNode *expr = ExprNodeCreate(ExprKind_Number);
        expr->SrcToken = token;
        return expr;
    }

    if (token.Kind == TokenKind_Identifier) {
        ExprNode *expr = ExprNodeCreate(ExprKind_Identifier);
        expr->SrcToken = token;
        return expr;
    }

    if (start) {
        for (int iter = 0; iter < ArrayCount(UnaryOperatorStartInput); ++iter) {
            if (token.Kind == UnaryOperatorStartInput[iter]) {
                ExprNode *expr = ExprNodeCreate(ExprKind_UnaryOperator);
                expr->Left = ParseSubexpression(t, false);
                expr->UnaryOperator = UnaryOperatorStartOutput[iter];
                expr->SrcToken = token;
                return expr;
            }
        }
    }

    for (int iter = 0; iter < ArrayCount(UnaryOperatorInput); ++iter) {
        if (token.Kind == UnaryOperatorInput[iter]) {
            ExprNode *expr = ExprNodeCreate(ExprKind_UnaryOperator);
            expr->Left = ParseSubexpression(t, false);
            expr->UnaryOperator = UnaryOperatorStartOutput[iter];
            expr->SrcToken = token;
            return expr;
        }
    }

    // HW: support for braces

    printf("error: expected operand\n");
    exit(1);
}

ExprNode *ParseExpression(Tokenizer *t, bool start) {
    ExprNode *left = ParseSubexpression(t, true);

    Token token = {};
    while (Tokenize(t, &token)) {
        if (start) {
            if (token.Kind == TokenKind_Equal) {
                ExprNode *expr = ExprNodeCreate(ExprKind_Assignment);
                expr->Left = left;
                expr->Right = ParseExpression(t, false);
                expr->SrcToken = token;
                return expr;
            }
        }

        start = false;
        bool invalid_op = true;

        ExprNode *expr = ExprNodeCreate(ExprKind_BinaryOperator);
        expr->Left = left;
        expr->SrcToken = token;

        for (int iter = 0; iter < ArrayCount(BinaryOperatorInput); ++iter) {
            if (token.Kind == BinaryOperatorInput[iter]) {
                expr->BinaryOperator = BinaryOperatorOutput[iter];
                invalid_op = false;
                break;
            }
        }

        if (invalid_op) {
            break;
        }

        expr->Right = ParseSubexpression(t, false);
        left = expr;
    }

    if (token.Kind == TokenKind_Semicolon) {
        return left;
    }

    printf("error: expected semicolon\n");
    exit(1);
}

void PrintExpr(ExprNode *expr, int indent) {
    for (int iter = 0; iter < indent; ++iter) {
        printf("    ");
    }

    if (expr->Kind == ExprKind_Number) {
        printf("Number: %f\n", expr->SrcToken.Number);
        return;
    }

    if (expr->Kind == ExprKind_Identifier) {
        printf("Identifier: %s\n", expr->SrcToken.Identifier);
        return;
    }

    if (expr->Kind == ExprKind_UnaryOperator) {
        printf("UnaryOperator: %s\n", TokenNames[expr->SrcToken.Kind]);
        PrintExpr(expr->Left, indent + 1);
        return;
    }

    if (expr->Kind == ExprKind_BinaryOperator) {
        printf("BinaryOperator: %s\n", TokenNames[expr->SrcToken.Kind]);
        PrintExpr(expr->Left, indent + 1);
        PrintExpr(expr->Right, indent + 1);
        return;
    }

    if (expr->Kind == ExprKind_Assignment) {
        printf("Assignment: %s\n", TokenNames[expr->SrcToken.Kind]);
        PrintExpr(expr->Left, indent + 1);
        PrintExpr(expr->Right, indent + 1);
        return;
    }
}

void Evaluate(ExprNode *expr) {
    printf("(todo)\n");
}

int main() {
    const char *input = "x = - y + 2 * c; z = x * 2;";

    Tokenizer t;
    t.Input = input;
    t.Position = 0;
    t.Length = strlen(input);

    while (t.Position < t.Length) {
        ExprNode *expr = ParseExpression(&t, true);
        PrintExpr(expr, 0);
        Evaluate(expr);
        ExprNodeReset();
        printf("=================================================================================\n");
    }

    return 0;
}
