%option noyywrap
%option nounput
%option noinput
%top{
    #include <stdarg.h>
    #include "common.h"
    #include "parser.h"
    #include <map>
    #include <math.h>
    using namespace std;
}
%{
    extern dump_type_t dump_type;

    inline void dump_tokens(const char* format, ...){
        va_list args; // 定义一个可变参数列表
        va_start(args, format); // 初始化可变参数列表
        if (dump_type == TOKENS) // 如果 dump_type 为 TOKENS
            vfprintf(yyout, format, args); // 将格式化的字符串输出到 yyout 流中
        va_end(args); // 结束可变参数列表
    }
    /* Your code here, if desired (lab3). */

%}

/* definitions section */
INTEGER ([1-9][0-9]*|0)
OCTAL (0[0-7][0-7]*)
HEXAL (0(x|X)[0-9a-fA-F][0-9a-fA-F]*)
FLOATING ((([0-9]*[.][0-9]*([eE][+-]?[0-9]+)?)|([0-9]+[eE][+-]?[0-9]+))[fLlL]?)
HEXADECIMAL_FLOAT (0[xX](([0-9A-Fa-f]*[.][0-9A-Fa-f]*([pP][+-]?[0-9]+)?)|([0-9A-Fa-f]+[pP][+-]?[0-9]+))[fLlL]?)
ID [[:alpha:]_][[:alpha:][:digit:]_]*

/* 结束符 */
EOL (\r\n|\n|\r)
/* 空白符 */
WHITE [\t ]
/* 单行注释, //开始后匹配任意非\n字符 */
COMMENT (\/\/[^\n]*)
/* 多行注释 */
commentbegin "/*"
/* 任意字符都有效 */
commentelement .
commentline \n
commentend "*/"
%x COMMENT

/*  Your code here (lab3). */

%%
    /* rules section */

{commentbegin} {BEGIN COMMENT;}
<COMMENT>{commentelement} {}
<COMMENT>{commentline} {yylineno++;}
    /* 切换为初始状态 */
<COMMENT>{commentend} {BEGIN INITIAL;}

{COMMENT}

"int" {
    //dump_tokens("INT\t%s\n", yytext);
    return TYPE_INT;
}

"float" {
    //dump_tokens("FLOAT\t%s\n", yytext);
    return TYPE_FLOAT;
}

"void" {
    //dump_tokens("VOID\t%s\n", yytext);
    return TYPE_VOID;
}

"const" {
    //dump_tokens("CONST\t%s\n", yytext);
    return CONST;
}

"while" {
    //dump_tokens("WHILE\t%s\n", yytext);
    return WHILE;
}

"break" {
    //dump_tokens("BREAK\t%s\n", yytext);
    return BREAK;
}

"continue" {
    //dump_tokens("CONTINUE\t%s\n", yytext);
    return CONTINUE;
}

"if" {
    //dump_tokens("IF\t%s\n", yytext);
    return IF;
}

"else" {
    //dump_tokens("ELSE\t%s\n", yytext);
    return ELSE;
}

"return" {
    //dump_tokens("RETURN\t%s\n", yytext);
    return RETURN;
}

"=" {
    //dump_tokens("ASSIGN\t%s\n", yytext);
    return ASSIGN;
}

"==" {
    //dump_tokens("EQUAL\t%s\n", yytext);
    return EQUAL;
}

"!=" {
    //dump_tokens("NOTUAL\t%s\n", yytext);
    return NOTEQUAL;
}

"<" {
    //dump_tokens("LESS\t%s\n", yytext);
    return LESS;
}

"<=" {
    //dump_tokens("LESSEQUAL\t%s\n", yytext);
    return LESSEQUAL;
}

">" {
    //dump_tokens("GREATER\t%s\n", yytext);
    return GREATER;
}

">=" {
    //dump_tokens("GREATEREQUAL\t%s\n", yytext);
    return GREATEREQUAL;
}

"+" {
    //dump_tokens("ADD\t%s\n", yytext);
    return ADD;
}

"-" {
    //dump_tokens("SUB\t%s\n", yytext);
    return SUB;
}

"*" {
    //dump_tokens("MUL\t%s\n", yytext);
    return MUL;
}

"/" {
    //dump_tokens("DIV\t%s\n", yytext);
    return DIV;
}

"%" {
    //dump_tokens("MOD\t%s\n", yytext);
    return MOD;
}

"||" {
    //dump_tokens("OR\t%s\n", yytext);
    return OR;
}

"&&" {
    //dump_tokens("AND\t%s\n", yytext);
    return AND;
}

"!" {
    //dump_tokens("NOT\t%s\n", yytext);
    return NOT;
}

"," {
    //dump_tokens("COMMA\t%s\n", yytext);
    return COMMA;
}

";" {
    //dump_tokens("SEMICOLON\t%s\n", yytext);
    return SEMICOLON;
}

"(" {
    //dump_tokens("LPAREN\t%s\n", yytext);
    return LPAREN;
}

")" {
    //dump_tokens("RPAREN\t%s\n", yytext);
    return RPAREN;
}

"[" {
    //dump_tokens("LBRACKET\t%s\n", yytext);
    return LBRACKET;
}

"]" {
    //dump_tokens("RBRACKET\t%s\n", yytext);
    return RBRACKET;
}

"{" {
    //dump_tokens("LBRACE\t%s\n", yytext);
    return LBRACE;
}

"}" {
    //dump_tokens("RBRACE\t%s\n", yytext);
    return RBRACE;
}
"getint" {
    
    //dump_tokens("ID\t%s\n", yytext);
    char *lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    if(identifiers->lookup(yytext)==nullptr){//符号表内未找到，插入
        Type* funcType = new FunctionType(TypeSystem::intType, {});//返回类型int，无参数
        SymbolTable* globalSymbolTable = identifiers; // 全局符号表
        while(globalSymbolTable->getPrev() != nullptr) {globalSymbolTable = globalSymbolTable->getPrev();}
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0); // 设置为全局
        globalSymbolTable->install(yytext, entry);
    }
    return ID;
}

"getch" {
    
    //dump_tokens("ID\t%s\n", yytext);
    char *lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    if(identifiers->lookup(yytext)==nullptr){//符号表内未找到，插入
        Type* funcType = new FunctionType(TypeSystem::intType, {});//返回类型int，无参数
        SymbolTable* globalSymbolTable = identifiers; // 全局符号表
        while(globalSymbolTable->getPrev() != nullptr) {globalSymbolTable = globalSymbolTable->getPrev();}
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0); // 设置为全局
        globalSymbolTable->install(yytext, entry);
    }
    return ID;
}
"getfloat" {
    
    //dump_tokens("ID\t%s\n", yytext);
    char *lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    if(identifiers->lookup(yytext)==nullptr){//符号表内未找到，插入
        Type* funcType = new FunctionType(TypeSystem::floatType, {});//返回类型float，无参数
        SymbolTable* globalSymbolTable = identifiers; // 全局符号表
        while(globalSymbolTable->getPrev() != nullptr) {globalSymbolTable = globalSymbolTable->getPrev();}
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0);// 设置为全局
        globalSymbolTable->install(yytext, entry);
    }
    return ID;
}
"getarray" {
    
    //dump_tokens("ID\t%s\n", yytext);
    char *lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    if(identifiers->lookup(yytext)==nullptr){//符号表内未找到，插入
        vector<Type*> vec; //形参类型表
        Type* arrayType = new IntArrayType();
        ((IntArrayType*)arrayType)->pushBackDimension(-1);// 第一个纬度大小初始化为-1
        vec.push_back(arrayType);
        Type* funcType = new FunctionType(TypeSystem::intType, vec);//返回类型int，参数int array
        SymbolTable* globalSymbolTable = identifiers; // 全局符号表
        while(globalSymbolTable->getPrev() != nullptr) {globalSymbolTable = globalSymbolTable->getPrev();}
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0);
        globalSymbolTable->install(yytext, entry);
    }
    return ID;
}
"getfarray" {
    
    //dump_tokens("ID\t%s\n", yytext);
    char *lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    if(identifiers->lookup(yytext)==nullptr){//符号表内未找到，插入
        vector<Type*> vec; //形参类型表
        Type* arrayType = new FloatArrayType();
        ((FloatArrayType*)arrayType)->pushBackDimension(-1);// 第一个纬度大小初始化为-1
        vec.push_back(arrayType);
        Type* funcType = new FunctionType(TypeSystem::intType, vec);//返回类型int，参数float array
        SymbolTable* globalSymbolTable = identifiers; // 全局符号表
        while(globalSymbolTable->getPrev() != nullptr) {globalSymbolTable = globalSymbolTable->getPrev();}
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0);
        globalSymbolTable->install(yytext, entry);
    }
    return ID;
}


"putint" {
    //dump_tokens("ID\t%s\n", yytext);
    char *lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    if(identifiers->lookup(yytext)==nullptr){//符号表内未找到，插入
        vector<Type*> vec; //形参类型表
        vec.push_back(TypeSystem::intType);
        Type* funcType = new FunctionType(TypeSystem::voidType, vec);//返回类型void，参数int
        SymbolTable* globalSymbolTable = identifiers; // 全局符号表
        while(globalSymbolTable->getPrev() != nullptr) {globalSymbolTable = globalSymbolTable->getPrev();}
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0);
        globalSymbolTable->install(yytext, entry);
    }
    return ID;
}
"putch" {
    
    //dump_tokens("ID\t%s\n", yytext);
    char *lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    if(identifiers->lookup(yytext)==nullptr){//符号表内未找到，插入
        vector<Type*> vec; //形参类型表
        vec.push_back(TypeSystem::intType);
        Type* funcType = new FunctionType(TypeSystem::voidType, vec);//返回类型void，参数int
        SymbolTable* globalSymbolTable = identifiers; // 全局符号表
        while(globalSymbolTable->getPrev() != nullptr) {globalSymbolTable = globalSymbolTable->getPrev();}
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0);
        globalSymbolTable->install(yytext, entry);
    }
    return ID;
}
"putfloat" {
    
    //dump_tokens("ID\t%s\n", yytext);
    char *lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    if(identifiers->lookup(yytext)==nullptr){//符号表内未找到，插入
        vector<Type*> vec; //形参类型表
        vec.push_back(TypeSystem::floatType);
        Type* funcType = new FunctionType(TypeSystem::voidType, vec);//返回类型void，参数float
        SymbolTable* globalSymbolTable = identifiers; // 全局符号表
        while(globalSymbolTable->getPrev() != nullptr) {globalSymbolTable = globalSymbolTable->getPrev();}
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0);
        globalSymbolTable->install(yytext, entry);
    }
    return ID;
}
"putarray" {
    
    //dump_tokens("ID\t%s\n", yytext);
    char *lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    if(identifiers->lookup(yytext)==nullptr){//符号表内未找到，插入
        vector<Type*> vec; //形参类型表
        vec.push_back(TypeSystem::intType);//int
        Type* arrayType = new IntArrayType();
        ((IntArrayType*)arrayType)->pushBackDimension(-1);//int array,第一个纬度大小初始化为-1
        vec.push_back(arrayType);
        Type* funcType = new FunctionType(TypeSystem::voidType, vec);//返回类型void，参数表int, int array
        SymbolTable* globalSymbolTable = identifiers; // 全局符号表
        while(globalSymbolTable->getPrev() != nullptr) {globalSymbolTable = globalSymbolTable->getPrev();}
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0);
        globalSymbolTable->install(yytext, entry);
    }
    return ID;
}
"putfarray" {
    
    //dump_tokens("ID\t%s\n", yytext);
    char *lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    if(identifiers->lookup(yytext)==nullptr){//符号表内未找到，插入
        vector<Type*> vec; //形参类型表
        vec.push_back(TypeSystem::intType);//int
        Type* arrayType = new FloatArrayType();
        ((FloatArrayType*)arrayType)->pushBackDimension(-1);//float array，第一个纬度大小初始化为-1
        vec.push_back(arrayType);
        Type* funcType = new FunctionType(TypeSystem::voidType, vec);//返回类型void，参数表int, float array
        SymbolTable* globalSymbolTable = identifiers; // 全局符号表
        while(globalSymbolTable->getPrev() != nullptr) {globalSymbolTable = globalSymbolTable->getPrev();}
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0);
        globalSymbolTable->install(yytext, entry);
    }
    return ID;
}

"starttime" {
    //dump_tokens("ID\t%s\n", yytext);
    char *lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    if(identifiers->lookup(yytext)==nullptr){//符号表内未找到，插入
        Type* funcType = new FunctionType(TypeSystem::voidType, {});//返回类型int，无参数
        SymbolTable* globalSymbolTable = identifiers; // 全局符号表
        while(globalSymbolTable->getPrev() != nullptr) {globalSymbolTable = globalSymbolTable->getPrev();}
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0); // 设置为全局
        globalSymbolTable->install(yytext, entry);
    }
    return ID;
}

"stoptime" {
    //dump_tokens("ID\t%s\n", yytext);
    char *lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    if(identifiers->lookup(yytext)==nullptr){//符号表内未找到，插入
        Type* funcType = new FunctionType(TypeSystem::voidType, {});//返回类型int，无参数
        SymbolTable* globalSymbolTable = identifiers; // 全局符号表
        while(globalSymbolTable->getPrev() != nullptr) {globalSymbolTable = globalSymbolTable->getPrev();}
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0); // 设置为全局
        globalSymbolTable->install(yytext, entry);
    }
    return ID;
}

"beforemain" {
    //dump_tokens("ID\t%s\n", yytext);
    char *lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    if(identifiers->lookup(yytext)==nullptr){//符号表内未找到，插入
        Type* funcType = new FunctionType(TypeSystem::voidType, {});//返回类型int，无参数
        SymbolTable* globalSymbolTable = identifiers; // 全局符号表
        while(globalSymbolTable->getPrev() != nullptr) {globalSymbolTable = globalSymbolTable->getPrev();}
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0); // 设置为全局
        globalSymbolTable->install(yytext, entry);
    }
    return ID;
}

"aftermain" {
    //dump_tokens("ID\t%s\n", yytext);
    char *lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    if(identifiers->lookup(yytext)==nullptr){//符号表内未找到，插入
        Type* funcType = new FunctionType(TypeSystem::voidType, {});//返回类型int，无参数
        SymbolTable* globalSymbolTable = identifiers; // 全局符号表
        while(globalSymbolTable->getPrev() != nullptr) {globalSymbolTable = globalSymbolTable->getPrev();}
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0); // 设置为全局
        globalSymbolTable->install(yytext, entry);
    }
    return ID;
}

{INTEGER} {
    int integer;
    integer = atoi(yytext);
    //dump_tokens("INTEGER\t%s\t%d\n", yytext, integer);
    yylval.itype = integer;
    return INTEGER;
}

{FLOATING} {
    float floating;
    sscanf(yytext, "%f", &floating);
    //dump_tokens("FLOATING\t%s\t%f\n", yytext, floating);
    yylval.ftype = double(floating);
    return FLOATING;
}

{HEXADECIMAL_FLOAT} {
    float hexfloat;
    sscanf(yytext, "%f", &hexfloat);
    //dump_tokens("HEXFLOATING\t%s\t%f\n", yytext, hexfloat);
    yylval.ftype = double(hexfloat);
    return FLOATING;
}

{OCTAL} {
    char* s;
    int oct = (int)strtol(yytext, &s, 8);
    //dump_tokens("OCTAL\t%s\t%d\n", yytext, oct);
    yylval.itype = oct;
    return INTEGER;
}

{HEXAL} {
    char* s;
    int hex = (int)strtol(yytext, &s, 16);
    //dump_tokens("DECIMAL\t%s\t%d\n", yytext, hex);
    yylval.itype = hex;
    return INTEGER;
}


{ID} {
    char *lexeme;
    //dump_tokens("ID\t%s\n", yytext);
    lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    return ID;
}

{EOL} yylineno++;

{WHITE} 
    /*  Your code here (lab3). */
%%
/* user code section */