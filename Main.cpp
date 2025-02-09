
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
    TokenKind_Comma,      // ,
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

const char* TokenNames[] = {
    "invalid", "number", "identifier", "=", ";", "+", "-", "*", "/", "(", ")", "|", "&", "~", "==", "||", "&&"
};

struct Token {
    TokenKind Kind;
    char      Identifier[MAX_IDENTIFIER_SIZE];
    int       IdentifierLen;
    double    Number;
};

struct Tokenizer {
    const char* Input;
    int         Position;
    int         Length;
};

bool SkipWhitespaces(Tokenizer* tokenizer) {
    char ch = tokenizer->Input[tokenizer->Position];
    if (!isspace(ch)) return false;

    for (; tokenizer->Position < tokenizer->Length; ++tokenizer->Position) {
        ch = tokenizer->Input[tokenizer->Position];
        if (!isspace(ch)) break;
    }

    return true;
}

const char      SingleCharTokenInput[] = { '=', ';', ',', '+', '-', '*', '/', '(', ')', '|', '&', '~'};
const TokenKind SingleCharTokenOutput[] = {
    TokenKind_Equal,
    TokenKind_Semicolon,
    TokenKind_Comma,
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

bool Tokenize(Tokenizer* tokenizer, Token* token) {
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

        char* endptr = 0;
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

void PrintToken(const Token* token) {
    printf("    %s ", TokenNames[token->Kind]);

    if (token->Kind == TokenKind_Number) {
        printf("(%f)", token->Number);
    }
    else if (token->Kind == TokenKind_Identifier) {
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

const int BinaryOperatorPrecedence[] = {
    0,
    0,
    1,
    1,
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
    ExprKind_Function
};

struct ExprNode {
    ExprKind Kind;
    ExprNode* Left;
    ExprNode* Right;
    BinaryOperatorKind BinaryOperator;
    UnaryOperatorKind UnaryOperator;
    Token SrcToken;
};

static ExprNode ExprNodeBuffers[8192];
static int ExprNodePos = 0;

struct Variable {
    char Name[MAX_IDENTIFIER_SIZE];
    double Value;
};
struct Memory {
    double Ans;
    Variable Vars[1024];
    int VariableCount;
};

ExprNode* ExprNodeCreate(ExprKind kind) {
    if (ExprNodePos == ArrayCount(ExprNodeBuffers)) {
        printf("error: out of memory");
        exit(1);
    }

    ExprNode* node = &ExprNodeBuffers[ExprNodePos];
    ExprNodePos++;
    memset(node, 0, sizeof(*node));
    node->Kind = kind;
    return node;
}

void ExprNodeReset() {
    ExprNodePos = 0;
}

struct Parser {
    Tokenizer Lex;
    Token     Current;
    bool      Parsing;
};

bool Parsing(Parser *parser) {
    return parser->Parsing;
}
struct Function {
    char Name[MAX_IDENTIFIER_SIZE];
    double Value[50];
    int ArgLength;
};

void AdvanceToken(Parser *parser) {
    parser->Parsing = Tokenize(&parser->Lex, &parser->Current);
}

Token GetCurrentToken(Parser *parser) {
    return parser->Current;
}

bool PeekToken(Parser *parser, TokenKind kind) {
    return parser->Current.Kind == kind;
}

bool AcceptToken(Parser *parser, TokenKind kind, Token *token) {
    if (PeekToken(parser, kind)) {
        *token = parser->Current;
        AdvanceToken(parser);
        return true;
    }
    return false;
}

bool ExpectToken(Parser *parser, TokenKind kind) {
    Token token = {};
    return AcceptToken(parser, kind, &token);
}

ExprNode *ParseExpression(Parser *parser, bool start, int prec, Function *func, Memory* mem);

ExprNode* ParseSubexpression(Parser *parser, bool start, Function* func, Memory* mem) {
    Token token = {};

    if (AcceptToken(parser, TokenKind_Number, &token)) {
        ExprNode *expr = ExprNodeCreate(ExprKind_Number);
        expr->SrcToken = token;
        return expr;
    }

    if (AcceptToken(parser, TokenKind_Identifier, &token)) {
        ExprNode* expr = ExprNodeCreate(ExprKind_Identifier);
        expr->SrcToken = token;
        return expr;
    }

    if (ExpectToken(parser, TokenKind_OpenBrace)) {//For this there will be two possibilities
        ExprNode *expr = ParseExpression(parser, true, -1, func, mem);
        if (!ExpectToken(parser, TokenKind_CloseBrace)) {
            printf("error: expected close brace\n");
            exit(1);
        }
        return expr;
    }

    if (start) {
        for (int iter = 0; iter < ArrayCount(UnaryOperatorStartInput); ++iter) {
            if (AcceptToken(parser, UnaryOperatorStartInput[iter], &token)) {
                ExprNode* expr = ExprNodeCreate(ExprKind_UnaryOperator);
                expr->Left = ParseSubexpression(parser, false, func, mem);
                expr->UnaryOperator = UnaryOperatorStartOutput[iter];
                expr->SrcToken = token;
                return expr;
            }
        }
    }

    for (int iter = 0; iter < ArrayCount(UnaryOperatorInput); ++iter) {
        if (AcceptToken(parser, UnaryOperatorInput[iter], &token)) {
            ExprNode* expr = ExprNodeCreate(ExprKind_UnaryOperator);
            expr->Left = ParseSubexpression(parser, false, func, mem);
            expr->UnaryOperator = UnaryOperatorStartOutput[iter];
            expr->SrcToken = token;
            return expr;
        }
    }

    printf("error: expected operand\n");
    exit(1);
}
ExprNode* ParseFunction(Parser* parser, bool start, ExprNode* funcExpr, Function* func, Memory* mem);
ExprNode* ParseExpression(Parser* parser, bool start, int prev_prec,Function* func, Memory* mem) {
    ExprNode* left = ParseSubexpression(parser, true, func, mem);
    if (left->Kind == ExprKind_Identifier) {
        if (PeekToken(parser, TokenKind_OpenBrace)) {
            ExprNode* funcExpr = ExprNodeCreate(ExprKind_Function);
            strcpy_s(func->Name, left->SrcToken.Identifier);
            ParseFunction(parser, start, funcExpr, func, mem);
            return funcExpr;
        }
    }
    while (Parsing(parser)) {
        if (start) {
            Token token = {};
            if (AcceptToken(parser, TokenKind_Equal, &token)) {
                ExprNode* expr = ExprNodeCreate(ExprKind_Assignment);
                expr->Left = left;
                expr->Right = ParseExpression(parser, false, -1, func, mem);
                expr->SrcToken = token;
                return expr;
            }
        }
        start = false;

        int prec = 0;
        int binary_op = -1;

        for (int iter = 0; iter < ArrayCount(BinaryOperatorInput); ++iter) {
            if (PeekToken(parser, BinaryOperatorInput[iter])) {
                prec = BinaryOperatorPrecedence[iter];
                binary_op = iter;
                break;
            }
        }

        if (binary_op == -1 || prec <= prev_prec)
            break;

        ExprNode *expr = ExprNodeCreate(ExprKind_BinaryOperator);
        expr->Left = left;
        expr->BinaryOperator = BinaryOperatorOutput[binary_op];
        expr->SrcToken = GetCurrentToken(parser);

        AdvanceToken(parser);

        expr->Right = ParseExpression(parser, false, prec, func, mem);

        left = expr;
    }

    return left;
}

ExprNode *ParseRootExpression(Parser *parser, Function* func, Memory* mem) {
    ExprNode *expr = ParseExpression(parser, true, -1, func, mem);
    if (!ExpectToken(parser, TokenKind_Semicolon)) {
        printf("error: expected semicolon\n");
        exit(1);
    }
    return expr;
}



char * AppendName(const char * input, const char* postAppendString) {
    char newName[MAX_IDENTIFIER_SIZE];
    memset(newName, 0, sizeof(newName[MAX_IDENTIFIER_SIZE]));
    int i = 0;
    for (; i < strlen(input); i++) {
        newName[i] = input[i];
    }
    while (i < (strlen(input) + strlen(postAppendString))) {
        newName[i] = postAppendString[i];
    }
    return newName;
}

Variable SearchVar(Memory* mem, char * name) {
    for (int iter = 0; iter < ArrayCount(mem->Vars); iter++) {
        if (!strcmp(name, mem->Vars[iter].Name)) {
            return mem->Vars[iter];
        }
    }
    printf("The variable %s does not exists\n",name);
    exit(-1);
}

ExprNode* ParseFunction(Parser* parser, bool start,ExprNode* funcExpr, Function* func, Memory* mem) {
    Token token;
    
    if (!ExpectToken(parser, TokenKind_OpenBrace)) {
        printf("Function name should be followed by Open Brace\n");
        exit(-1);
    }
    if (!ExpectToken(parser, TokenKind_CloseBrace)) {
        for (int iter = 0;; iter++) {
            funcExpr->Left = ParseSubexpression(parser, true, func, mem);
            if (funcExpr->Left->Kind == ExprKind_Identifier || funcExpr->Left->Kind == ExprKind_Number) {
                if (funcExpr->Left->Kind == ExprKind_Identifier) {
                   func->Value[func->ArgLength++] = SearchVar(mem, funcExpr->Left->SrcToken.Identifier).Value;
                }
                else if (funcExpr->Left->Kind == ExprKind_Number) {
                    func->Value[func->ArgLength++] = parser->Current.Number;
                }
                if (func->ArgLength >= 50) {
                    printf("Only upto 50 arguments are supported\n");
                    exit(-1);
                }
                if (ExpectToken(parser, TokenKind_Comma)) {
                    funcExpr = funcExpr->Left;
                    continue;
                }
                else if (!ExpectToken(parser, TokenKind_CloseBrace)) {
                    printf("Expected Close Brace\n");
                    exit(-1);
                }
                return funcExpr;
                //I am thinking to eliminate the usuage of semicolon.
            }
            else {
                printf("Expected arguments are either Variables or Numbers\n");
                exit(-1);
            }
        }
        
    }
    else if (ExpectToken(parser, TokenKind_CloseBrace)) {
        return funcExpr;
    }
    printf("Expected Close Braces\n");
    exit(-1);
}
double Sum(Function func) {
    double d = 0;
    for (int iter = 0; iter < func.ArgLength; iter++) {
        d = d + func.Value[iter];
    }
    return d;
}
double Sub(Function func) {
    double d = func.Value[0];
    for (int iter = 1; iter < func.ArgLength; iter++) {
        d = d - func.Value[iter];
    }
    return d;
}double Mul(Function func) {
    double d = 1;
    for (int iter = 0; iter < func.ArgLength; iter++) {
        d = d * func.Value[iter];
    }
    return d;
}double Div(Function func) {
    double d = 1;
    for (int iter = 0; iter < func.ArgLength; iter++) {
        d = d / func.Value[iter];
    }
    return d;
}
void checkFunction(Function* func, Memory *mem) {
    if (!strcmp(func->Name , "sum")) {
        mem->Ans = Sum(*func);
    }
    else if (!strcmp(func->Name , "sub")) {
        mem->Ans = Sub(*func);
    }
    else if (!strcmp(func->Name , "mul")) {
        mem->Ans = Mul(*func);
    }
    else if (!strcmp(func->Name , "div")) {
        mem->Ans = Div(*func);
    }
    else {
        printf("%s function is not supported yet\n", func->Name);
        exit(-1);
    }
    func->ArgLength = 0;

}

void PrintExpr(ExprNode* expr, int indent) {
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

double Evaluate(ExprNode* expr,Function* func, Memory *mem) {
    if (expr->Kind == ExprKind_Number) {
        return expr->SrcToken.Number;
    }
    else if (expr->Kind == ExprKind_Identifier) {
        for (int iter = 0; iter <= 1024; iter++) {
            if (!strcmp(expr->SrcToken.Identifier, mem->Vars[iter].Name)) {
                return mem->Vars[iter].Value;
            }
        }
        printf("The variable %s is undefined\n",expr->SrcToken.Identifier);
        exit(-1);
    }
    else if (expr->Kind == ExprKind_UnaryOperator) {
        if (expr->UnaryOperator == UnaryOperator_Plus) {
           return Evaluate(expr->Left,func,mem);
        }
        else if (expr->UnaryOperator == UnaryOperator_Minus) {
            return -Evaluate(expr->Left,func, mem);
        }
        else {
            printf("TODO\n");
            return 0;
        }
    }
    else if (expr->Kind == ExprKind_BinaryOperator) {
        double L = Evaluate(expr->Left,func, mem);
        double R = Evaluate(expr->Right,func, mem);
        if (expr->BinaryOperator == BinaryOperator_Plus) {
            return (L + R);
        }
        else if (expr->BinaryOperator == BinaryOperator_Minus) {
            return (L - R);
        }
        else if (expr->BinaryOperator == BinaryOperator_Multiply) {
            return (L * R);
        }
        else if (expr->BinaryOperator == BinaryOperator_Divide) {
            return (L / R);
        }
        else {
            printf("TODO\n");
            return 0;
        }
    }
    else if (expr->Kind == ExprKind_Assignment) {
        if (expr->Left->SrcToken.Kind != TokenKind_Identifier) {
            printf("Left side of the Assignment can only be of variable type\n");
            exit(-1);
        }
        strcpy_s(mem->Vars[mem->VariableCount].Name , expr->Left->SrcToken.Identifier);
        mem->Vars[mem->VariableCount].Value = Evaluate(expr->Right,func, mem);
        return mem->Vars[mem->VariableCount++].Value;
    }
    else if (expr->Kind == ExprKind_Function) {
        checkFunction(func, mem);
        return mem->Ans;
    }
    else {
        printf("TODO\n");
        return 0;
    }
}

void EvaluateRootExpr(ExprNode* expr, Function* func, Memory* mem) {
    mem->Ans = Evaluate(expr, func, mem);
}


Parser StartParsing(const char *str, int length) {
    Parser parser = {};
    parser.Lex.Input = str;
    parser.Lex.Position = 0;
    parser.Lex.Length = length;
    AdvanceToken(&parser);
    return parser;
}

int main() {
    const char* input = "a=5; b=2; sum(a,b);";
    Parser parser = StartParsing(input, strlen(input));
    Memory memory;
    Function func;
    memset(&func, 0, sizeof(func));
    memset(&memory, 0, sizeof(memory));
    while (Parsing(&parser)) {
        ExprNode* expr = ParseRootExpression(&parser,&func,&memory);
        PrintExpr(expr, 0);
        EvaluateRootExpr(expr,&func, &memory);
        ExprNodeReset();
        printf("Result = %f\n", memory.Ans);
        printf("=================================================================================\n");
    }

    return 0;
}
