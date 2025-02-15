
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <float.h>
#include <math.h>
#include <stdint.h>

//////////////////////////////////////////////////////////////////////////
//////////////////////// Tokenizer  //////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

#define MAX_IDENTIFIER_SIZE 128
#define MAX_STRING_SIZE     256

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
    TokenKind_String,
    TokenKind_OpenCurlyBrace,
    TokenKind_CloseCurlyBrace,
};

const char *TokenNames[] = {
    "invalid", "number", "identifier", "=", ";", "+", "-", "*", "/", "(", ")", "|", "&", "~", "==", "||", "&&"
};

enum ValueType {
    ValueType_Void,
    ValueType_Int,
    ValueType_Float,
    ValueType_String,
};

struct Value {
    ValueType Kind;
    int64_t Int;
    double Float;
    char String[MAX_STRING_SIZE];
};

struct Token {
    TokenKind Kind;
    int       IdentifierLen;
    char Identifier[MAX_IDENTIFIER_SIZE];
    Value    Data;
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

const char      SingleCharTokenInput[] = { '=', ';', ',', '+', '-', '*', '/', '(', ')', '|', '&', '~' };
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
        if (a == '@' && b == '\"') {
            tokenizer->Position+=2;
            for (int iter = 0; tokenizer->Position < tokenizer->Length; iter++) {
                char c1 = tokenizer->Input[tokenizer->Position];
                char c2 = tokenizer->Position+1 < tokenizer->Length? tokenizer->Input[tokenizer->Position+1]:0;

                if ((c1 == '\"') && c2 == '@') {
                    tokenizer->Position+=2;
                    token->Kind = TokenKind_String;
                    return true;
                }
                if (iter >= MAX_STRING_SIZE) {
                    printf("Too long String\n");
                    exit(-1);
                }
                token->Data.String[iter] = c1;
                tokenizer->Position++;
            }
            printf("End quotation @ not found\n");
            exit(-1);
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

        char *endptr1 = 0;
        char *endptr2 = 0;
        Value number;
        number.Float = strtod(str, &endptr1);
        number.Int = strtol(str, &endptr2, 10);
        number.Kind = endptr1 == endptr2 ? ValueType_Int : ValueType_Float;

        if (str + len != endptr1) {
            printf("invalid number\n");
            exit(1);
        }

        token->Kind = TokenKind_Number;
        token->Data = number;

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
    if(a == '\"'){
        tokenizer->Position++;
        for (int iter = 0; tokenizer->Position < tokenizer->Length; iter++) {
            char b = tokenizer->Input[tokenizer->Position];
            if (( b == '\"')) {
                tokenizer->Position ++;
                token->Kind = TokenKind_String;
                return true;
            }
            if (iter >= MAX_STRING_SIZE) {
                printf("Too long String\n");
                exit(-1);
            }
            if (b == '\n' || b == '\r') {
                printf("Expected end quotation\n");
                exit(-1);
            }
            if (b == '\\') {
                tokenizer->Position++;
               char c =tokenizer->Position < tokenizer->Length? tokenizer->Input[tokenizer->Position]:0;
               if (c == 'n') {
                   b = '\n';
               }
               else if (c == 'r') {
                   b = '\r';
               }
               else if (c == 't') {
                   b = '\t';
               }
               else if (c == '\\') {
                   b = '\\';
               }
               else {
                   printf("Invalid Escape Sequence in String\n");
                   exit(-1);
               }
            }
            
            token->Data.String[iter] = b;
            tokenizer->Position++;
        }
        printf("End quotation not found\n");
        exit(-1);
    }
    printf("invalid character: %c\n", a);
    exit(1);

    return false;
}

//////////////////////////////////////////////////////////////////////////
/////////////////////////  Parser  ///////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

#define MAX_VARIABLE_COUNT 1024
#define MAX_ARGUMENT_COUNT 64

enum BinaryOperatorKind {
    BinaryOperator_Plus,
    BinaryOperator_Minus,
    BinaryOperator_Multiply,
    BinaryOperator_Divide,
    BinaryOperator_BitwiseAnd,
    BinaryOperator_BitwiseOr,
};

const TokenKind BinaryOperatorInput[] = {
    TokenKind_Plus,
    TokenKind_Minus,
    TokenKind_Multiply,
    TokenKind_Divide,
    TokenKind_BitwiseAnd,
    TokenKind_BitwiseOr,
};

const BinaryOperatorKind BinaryOperatorOutput[] = {
    BinaryOperator_Plus,
    BinaryOperator_Minus,
    BinaryOperator_Multiply,
    BinaryOperator_Divide,
    BinaryOperator_BitwiseAnd,
    BinaryOperator_BitwiseOr,
};

const int BinaryOperatorPrecedence[] = {
    1,
    1,
    2,
    2,
    0,
    0
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
    ExprKind_BuiltinFunc,
    ExprKind_String,
    ExprKind_UserDefinedFunc,
};

struct ExprNode;

enum BuiltinFunc {
    BuiltinFunc_Sum,
    BuiltinFunc_Mul,
    BuiltinFunc_Min,
    BuiltinFunc_Max,
    BuiltinFunc_Sin,
    BuiltinFunc_Cos,
    BuiltinFunc_Tan,
    BuiltinFunc_Print,
    BuiltinFunc_Count,
};

struct BuiltinFuncDesc {
    const char *Name;
    int ArgCount;
};

static BuiltinFuncDesc BuiltinFuncDescs[] = { 
    { "sum", -1 },
    { "mul", -1 },
    { "min", -1 },
    { "max", -1 },
    { "sin", 1 },
    { "cos", 1 }, 
    { "tan", 1 },
    {"print", -1},
};

struct FunctionArguments {
    ExprNode *Args[MAX_ARGUMENT_COUNT];
    int Count;
};

struct ExprNode {
    ExprKind Kind;
    Token SrcToken;
    ExprNode *Left;
    ExprNode *Right;
    BuiltinFunc Func;
    FunctionArguments FuncArgs;
    BinaryOperatorKind BinaryOperator;
    UnaryOperatorKind UnaryOperator;
};

static ExprNode ExprNodeBuffers[8192];
static int ExprNodePos = 0;

ExprNode *ExprNodeCreate(ExprKind kind, Token token) {
    if (ExprNodePos == ArrayCount(ExprNodeBuffers)) {
        printf("error: out of memory");
        exit(1);
    }

    ExprNode *node = &ExprNodeBuffers[ExprNodePos];
    ExprNodePos++;
    memset(node, 0, sizeof(*node));
    node->Kind = kind;
    node->SrcToken = token;
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

void AdvanceToken(Parser *parser) {
    parser->Parsing = Tokenize(&parser->Lex, &parser->Current);
    //Jaile pani yesle parser->Lex le next token point garira hunxa yesko matlab k ho vanedekhi ki parser->Current ma next token aauxa
}

Parser StartParsing(const char *str, int length) {
    Parser parser = {};
    parser.Lex.Input = str;
    parser.Lex.Position = 0;
    parser.Lex.Length = length;
    AdvanceToken(&parser);
    return parser;
}

Token GetCurrentToken(Parser *parser) {
    return parser->Current;
}

bool PeekToken(Parser *parser, TokenKind kind) {
    return parser->Current.Kind == kind;
}

bool AcceptToken(Parser *parser, TokenKind kind, Token *token) {
    if (PeekToken(parser, kind)) {
        *token = parser->Current;//This will hold the current token as the parser->curent will be updated on AdvanceToken call.
        AdvanceToken(parser);
        return true;
    }
    return false;
}

bool ExpectToken(Parser *parser, TokenKind kind) {
    Token token = {};
    return AcceptToken(parser, kind, &token);
}

ExprNode *ParseExpression(Parser *parser, bool start, int prec);

FunctionArguments ParseFunctionArguments(Parser *parser) {
    FunctionArguments args;
    memset(&args, 0, sizeof(args));

    if (PeekToken(parser, TokenKind_CloseBrace))
        return args;

    while (Parsing(parser)) {
        if (args.Count == MAX_ARGUMENT_COUNT) {
            printf("too many parameters in function\n");
            exit(-1);
        }

        args.Args[args.Count] = ParseExpression(parser, true, -1);
        args.Count += 1;

        if (!ExpectToken(parser, TokenKind_Comma))
            break;
    }

    return args;
}

ExprNode *ParseSubexpression(Parser *parser, bool start) {
    Token token = {};

    if (AcceptToken(parser, TokenKind_Number, &token)) {
        ExprNode *expr = ExprNodeCreate(ExprKind_Number, token);
        return expr;
    }
    if (AcceptToken(parser, TokenKind_Identifier, &token)) {
        if (ExpectToken(parser, TokenKind_OpenBrace)) {
            BuiltinFuncDesc *desc = NULL;
            BuiltinFunc func = BuiltinFunc_Sum;

            for (int iter = 0; iter < BuiltinFunc_Count; ++iter) {
                if (!strcmp(BuiltinFuncDescs[iter].Name, token.Identifier)) {
                    desc = &BuiltinFuncDescs[iter];
                    func = (BuiltinFunc)iter;
                    break;
                }
            }

            if (!desc) {
                printf("Creating User Defined Function...\n");
                ExprNode *expr = ExprNodeCreate(ExprKind_UserDefinedFunc, token);
                ExprNode *left = expr;
                expr->FuncArgs = ParseFunctionArguments(parser);
                if (!ExpectToken(parser, TokenKind_CloseBrace)) {
                    printf("expected close brace\n");
                    exit(-1);
                }
                if (!ExpectToken(parser, TokenKind_OpenCurlyBrace)) {
                    printf("Expected Open Curly Brace after function declaration\n");
                    exit(-1);
                }
                while (!ExpectToken(parser, TokenKind_CloseCurlyBrace)) {
                    if (parser->Lex.Position >= parser->Lex.Length) {
                        printf("Expected Closed Curly Braces to end the Function\n");
                        exit(-1);
                    }
                    left = ParseExpression(parser, true, -1);
                    left = left->Left;
                }
                expr->Left = left;
                return expr;
            }

            ExprNode *expr = ExprNodeCreate(ExprKind_BuiltinFunc, token);
            expr->FuncArgs = ParseFunctionArguments(parser);
            expr->Func = func;

            if (desc->ArgCount != -1 && desc->ArgCount != expr->FuncArgs.Count) {
                printf("expected %d number of arguments but got %d arguments for function %s\n",
                    desc->ArgCount, expr->FuncArgs.Count, desc->Name);
                exit(-1);
            }

            if (!ExpectToken(parser, TokenKind_CloseBrace)) {
                printf("expected close brace\n");
                exit(-1);
            }
            return expr;
        } else {
            ExprNode *expr = ExprNodeCreate(ExprKind_Identifier, token);
            return expr;
        }
    }

    if (ExpectToken(parser, TokenKind_OpenBrace)) {
        ExprNode *expr = ParseExpression(parser, true, -1);
        if (!ExpectToken(parser, TokenKind_CloseBrace)) {
            printf("error: expected close brace\n");
            exit(1);
        }
        return expr;
    }

    if (start) {
        for (int iter = 0; iter < ArrayCount(UnaryOperatorStartInput); ++iter) {
            if (AcceptToken(parser, UnaryOperatorStartInput[iter], &token)) {
                ExprNode *expr = ExprNodeCreate(ExprKind_UnaryOperator, token);
                expr->Left = ParseSubexpression(parser, false);
                expr->UnaryOperator = UnaryOperatorStartOutput[iter];
                return expr;
            }
        }
    }

    for (int iter = 0; iter < ArrayCount(UnaryOperatorInput); ++iter) {
        if (AcceptToken(parser, UnaryOperatorInput[iter], &token)) {
            ExprNode *expr = ExprNodeCreate(ExprKind_UnaryOperator, token);
            expr->Left = ParseSubexpression(parser, false);
            expr->UnaryOperator = UnaryOperatorStartOutput[iter];
            return expr;
        }
    }
    if (AcceptToken(parser, TokenKind_String, &token)) {
      //parser will contain the current token that has been parsed and the toekn will contain the previous token
        //Here the previous token is TokenKind_DoubleQuotation
        ExprNode *expr = ExprNodeCreate(ExprKind_String, token);
        return expr;
    }

    printf("error: expected operand\n");
    exit(1);
}

ExprNode *ParseExpression(Parser *parser, bool start, int prev_prec) {
    ExprNode *left = ParseSubexpression(parser, true);

    while (Parsing(parser)) {
        if (start) {
            Token token = {};
            if (AcceptToken(parser, TokenKind_Equal, &token)) {
                if (left->Kind != ExprKind_Identifier) {
                    printf("The left side of the assignment should be an identifier\n");
                    exit(-1);
                }
                ExprNode *expr = ExprNodeCreate(ExprKind_Assignment, token);
                expr->Left = left;
                expr->Right = ParseExpression(parser, false, -1);
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

        ExprNode *expr = ExprNodeCreate(ExprKind_BinaryOperator, GetCurrentToken(parser));
        expr->Left = left;
        expr->BinaryOperator = BinaryOperatorOutput[binary_op];

        AdvanceToken(parser);

        expr->Right = ParseExpression(parser, false, prec);

        left = expr;
    }

    return left;
}

ExprNode *ParseRootExpression(Parser *parser) {
    ExprNode *expr = ParseExpression(parser, true, -1);
    if (!ExpectToken(parser, TokenKind_Semicolon)) {
        printf("error: expected semicolon\n");
        exit(1);
    }
    return expr;
}

//////////////////////////////////////////////////////////////////////////
/////////////////////////  Print  ////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

void PrintExpr(ExprNode *expr, int indent) {
    for (int iter = 0; iter < indent; ++iter) {
        printf("    ");
    }
   
    if (expr->Kind == ExprKind_Number) {
        Value num = expr->SrcToken.Data;
        if (num.Kind == ValueType_Float) {
            printf("Float = %f\n", num.Float);
        }
        else {
            printf("Int = %ld\n", num.Int);
        }
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

    if (expr->Kind == ExprKind_BuiltinFunc) {
        printf("Function: %s\n", expr->SrcToken.Identifier);
        for (int iter = 0; iter < expr->FuncArgs.Count; ++iter)
            PrintExpr(expr->FuncArgs.Args[iter], indent + 1);
        return;
    }
    if (expr->Kind == ExprKind_String) {
        printf("String: \"%s\"\n", expr->SrcToken.Data.String);
        return;
    }
    printf("TODO\n");
}

//////////////////////////////////////////////////////////////////////////
/////////////////////////  Evalute  //////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

struct Variable {
    char Name[MAX_IDENTIFIER_SIZE];
    Value Value;
};

struct Memory {
    Value Ans;
    Variable Vars[MAX_VARIABLE_COUNT];
    int VariableCount;
};

Variable *SearchVar(Memory *mem, char *varName) {
    for (int iter = 0; iter < mem->VariableCount; iter++) {
        if (!strcmp(varName, mem->Vars[iter].Name)) {
            return &mem->Vars[iter];
        }
    }
    return NULL;
}

Variable *GetVariable(Memory *mem, char *varName) {
    Variable *var = SearchVar(mem, varName);
    if (!var) {
        if (mem->VariableCount == MAX_VARIABLE_COUNT) {
            printf("Out of Memory. Cannot create new variable\n");
            exit(-1);
        }
        strcpy_s(mem->Vars[mem->VariableCount].Name, varName);
        var = &mem->Vars[mem->VariableCount++];
    }
    return var;
}

Value AddValue(Value A, Value B) {
    Value R = {};
    R.Int = A.Int + B.Int;
    R.Float = A.Float + B.Float;
    R.Kind = A.Kind != B.Kind ? ValueType_Float : A.Kind;
    return R;
}

Value SubValue(Value A, Value B) {
    Value R = {};
    R.Int = A.Int - B.Int;
    R.Float = A.Float - B.Float;
    R.Kind = A.Kind != B.Kind ? ValueType_Float : A.Kind;
    return R;
}

Value MulValue(Value A, Value B) {
    Value R = {};
    R.Int = A.Int * B.Int;
    R.Float = A.Float * B.Float;
    R.Kind = A.Kind != B.Kind ? ValueType_Float : A.Kind;
    return R;
}

Value DivValue(Value A, Value B) {
    Value R = {};
    R.Int = A.Int / B.Int;
    R.Float = A.Float / B.Float;
    R.Kind = A.Kind != B.Kind ? ValueType_Float : A.Kind;
    return R;
}

Value Evaluate(ExprNode *expr, Memory *mem);

Value Sum(FunctionArguments *args, Memory *mem) {
    Value d = {};
    for (int iter = 0; iter < args->Count; iter++) {
        d = AddValue(d,Evaluate(args->Args[iter],mem));
    }
    return d;
}

Value Mul(FunctionArguments *args, Memory *mem) {
    Value d = {};
    d.Int = 1;
    d.Float = 1;
    d.Kind = ValueType_Int;
    for (int iter = 0; iter < args->Count; iter++) {
        d = MulValue(d, Evaluate(args->Args[iter], mem));
    }
    return d;
}

Value Min(FunctionArguments *args, Memory *mem) {
    Value d = {};
    d.Float= DBL_MAX;
    d.Int = INT64_MAX;
    d.Kind = ValueType_Int;
    for (int iter = 0; iter < args->Count; iter++) {
        Value n = Evaluate(args->Args[iter], mem);
        if (n.Float < d.Float) d = n;
    }
    return d;
}

Value Max(FunctionArguments *args, Memory *mem) {
    Value d = {};
    d.Float = DBL_MAX;
    d.Int = INT64_MAX;
    d.Kind = ValueType_Int;
    for (int iter = 0; iter < args->Count; iter++) {
        Value n = Evaluate(args->Args[iter], mem);
        if (n.Float > d.Float) d = n;
    }
    return d;
}

Value Sin(FunctionArguments *args, Memory *mem) {
    Value d = Evaluate(args->Args[0], mem);
    Value r;
    r.Float = sin(d.Float);
    r.Kind = ValueType_Float;
    r.Int = (int64_t)r.Float;
    return r;
}

Value Cos(FunctionArguments *args, Memory *mem) {
    Value d = Evaluate(args->Args[0], mem);
    Value r;
    r.Float = cos(d.Float);
    r.Kind = ValueType_Float;
    r.Int = (int64_t)r.Float;
    return r;
}

Value Tan(FunctionArguments *args, Memory *mem) {
    Value d = Evaluate(args->Args[0], mem);
    Value r;
    r.Float = tan(d.Float);
    r.Kind = ValueType_Float;
    r.Int = (int64_t)r.Float;
    return r;
}
Value Print(FunctionArguments *args, Memory *mem) {
    for (int iter = 0; iter < args->Count; iter++) {
       Value d = Evaluate(args->Args[iter], mem);
        if (d.Kind == ValueType_Float) {
            printf("%f ", d.Float);
        }
        else if (d.Kind == ValueType_Int) {
            printf("%ld ", d.Int);
        }
        else if (d.Kind == ValueType_String) {
            printf("%s ", d.String);

        }
    }
    printf("\n");
    return Value{};
}

typedef Value (*EvaluateBuiltinFunc)(FunctionArguments *, Memory *);

static EvaluateBuiltinFunc BuildinFuncEvaluateTable[] = {
    Sum, Mul, Min, Max, Sin, Cos, Tan, Print
};

Value EvaluateBuildinFunction(ExprNode *expr, Memory *mem) {
    if (expr->Func < ArrayCount(BuildinFuncEvaluateTable))
        return BuildinFuncEvaluateTable[expr->Func](&expr->FuncArgs, mem);
    printf("TODO\n");
}

Value Evaluate(ExprNode *expr, Memory *mem) {
    if (expr->Kind == ExprKind_Number) {
        return expr->SrcToken.Data;
    }

    if (expr->Kind == ExprKind_Identifier) {
        Variable *var = SearchVar(mem, expr->SrcToken.Identifier);
        if (!var) {
            printf("Variable %s is not defined\n", expr->SrcToken.Identifier);
            exit(-1);
        }
        return var->Value;
    }

    if (expr->Kind == ExprKind_UnaryOperator) {
        if (expr->UnaryOperator == UnaryOperator_Plus) {
            return Evaluate(expr->Left, mem);
        } else if (expr->UnaryOperator == UnaryOperator_Minus) {
            Value v = Evaluate(expr->Left, mem);
            v.Int = -v.Int;
            v.Float = -v.Float;
            return v;
        } else {
            printf("TODO\n");
            return Value{};
        }
    }

    if (expr->Kind == ExprKind_BinaryOperator) {
        Value L = Evaluate(expr->Left, mem);
        Value R = Evaluate(expr->Right, mem);
        if (expr->BinaryOperator == BinaryOperator_Plus) {
            return AddValue(L, R);
        }
        else if (expr->BinaryOperator == BinaryOperator_Minus) {
            return SubValue(L, R);
        } 
        else if (expr->BinaryOperator == BinaryOperator_Multiply) {
            return MulValue(L, R);
        } 
        else if (expr->BinaryOperator == BinaryOperator_Divide) {
            return DivValue(L, R);
        }
        else if (expr->BinaryOperator == BinaryOperator_BitwiseAnd) {
            Value d = {};
            d.Int = L.Int & R.Int;
            d.Float = (int)L.Float & (int)R.Float;
            d.Kind = L.Kind != R.Kind ? ValueType_Float : L.Kind;
            return d;
        }
        else if (expr->BinaryOperator == BinaryOperator_BitwiseOr) {
            Value d = {};
            d.Int = L.Int | R.Int;
            d.Float = (int)L.Float | (int)R.Float;
            d.Kind = L.Kind != R.Kind ? ValueType_Float : L.Kind;
            return d;
        }
        else {
            printf("TODO\n");
            return Value{};
        }
    }

    if (expr->Kind == ExprKind_Assignment) {
        Variable *var = GetVariable(mem, expr->Left->SrcToken.Identifier);
        var->Value = Evaluate(expr->Right, mem);
        return var->Value;
    }

    if (expr->Kind == ExprKind_BuiltinFunc) {
        return EvaluateBuildinFunction(expr, mem);
    }

    if (expr->Kind == ExprKind_String) {
        Value d = {};
        strcpy_s(d.String, expr->SrcToken.Data.String);
        d.Kind = ValueType_String;
        return d;
    }
    if (expr->Kind == ExprKind_UserDefinedFunc) {
        return Evaluate(expr->Left, mem);
    }
    printf("TODO\n");
}

void EvaluateRootExpr(ExprNode *expr, Memory *mem) {
    mem->Ans = Evaluate(expr, mem);
}

char *ReadEntireFile(const char *filepath) {
    FILE *fp = fopen(filepath, "rb");
    if (!fp) {
        printf("failed to open file: %s\n", filepath);
        exit(-1);
    }

    fseek(fp, 0, SEEK_END);
    long length = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char *input = (char *)malloc(length + 1);
    fread(input, length, 1, fp);
    fclose(fp);

    input[length] = 0;
    return input;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        printf("error: expected file path\n");
        exit(-1);
    }

    const char *filepath = argv[1];

    char *input = ReadEntireFile(filepath);

    Parser parser = StartParsing(input, strlen(input));

    Memory memory;
    memset(&memory, 0, sizeof(memory));

    while (Parsing(&parser)) {
        ExprNode *expr = ParseRootExpression(&parser);
        PrintExpr(expr, 0);
        EvaluateRootExpr(expr, &memory);
        ExprNodeReset();
        if (memory.Ans.Kind == ValueType_Float) {
            printf("Float = %f\n", memory.Ans.Float);
        }
        else if(memory.Ans.Kind == ValueType_Int){
            printf("Int = %ld\n", memory.Ans.Int);
        }
        else if (memory.Ans.Kind == ValueType_String) {
            printf("String = \"%s\"\n", memory.Ans.String);
        }
        printf("=================================================================================\n");
    }
    return 0;
}


/*
Now I need to develop the support for the string types and now the string types is in the variable so I need to change that 


*/