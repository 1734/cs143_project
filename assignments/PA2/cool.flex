/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */

%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

#include <string>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
        if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
                YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

static int nested_comment_layer;
extern "C" int yylex();

/*
 *  Add Your own definitions here
 */

%}

%x STRING INLINE_COMMENT NESTED_COMMENT

/*
 * Define names for regular expressions here.
 */

DARROW          =>

WHITESPACE      [ \f\r\t\v]

DIGITAL         [0-9]

%%

 /*
  *  The multiple-character operators.
  */
{DARROW} { return (DARROW); }
"<=" { return (LE); }
"<-" { return (ASSIGN); }

"+" { return int('+'); }
"-" { return int('-'); }
"*" { return int('*'); }
"/" { return int('/'); }
"(" { return int('('); }
")" { return int(')'); }
"<" { return int('<'); }
"=" { return int('='); }
"~" { return int('~'); }
"," { return int(','); }
"." { return int('.'); }
"{" { return int('{'); }
"}" { return int('}'); }
"@" { return int('@'); }

{WHITESPACE} {}

\n {
    curr_lineno++;
}

"*)" {
    cool_yylval.error_msg = "Unmatched *)";
    return (ERROR);
}

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */

(?i:class) { return (CLASS); }
(?i:else) { return (ELSE); }
f(?i:alse) { cool_yylval.boolean = 0; return (BOOL_CONST); }
(?i:fi) { return (FI); }
(?i:if) { return (IF); }
(?i:in) { return (IN); }
(?i:inherits) { return (INHERITS); }
(?i:isvoid) { return (ISVOID); }
(?i:let) { return (LET); }
(?i:loop) { return (LOOP); }
(?i:pool) { return (POOL); }
(?i:then) { return (THEN); }
(?i:while) { return (WHILE); }
(?i:case) { return (CASE); }
(?i:esac) { return (ESAC); }
(?i:new) { return (NEW); }
(?i:of) { return (OF); }
(?i:not) { return (NOT); }
t(?i:rue) { cool_yylval.boolean = 1; return (BOOL_CONST); }

[0-9]+ {
    cool_yylval.symbol = inttable.add_string(yytext);
    return (INT_CONST);
}

[a-z][a-zA-z0-9_]* {
    cool_yylval.symbol = idtable.add_string(yytext);
    return (OBJECTID);
}

[A-Z][a-zA-z0-9_]* {
    cool_yylval.symbol = idtable.add_string(yytext);
    return (TYPEID);
}

\" {
    BEGIN(STRING);
    yymore();
}

"(*" {
    nested_comment_layer = 1;
    BEGIN(NESTED_COMMENT);
}

. {
    return (ERROR);
}

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */

<STRING>[^\\\n\"]+ { yymore(); }

<STRING>\\[^\n] { yymore(); }

<STRING>\\\n {
    curr_lineno++;
    yymore();
}

<STRING>\\ {
    cool_yylval.error_msg = "EOF in string constant";
    BEGIN(INITIAL);
    yyrestart(yyin);
    return (ERROR);
}

<STRING>\n {
    cool_yylval.error_msg = "Unterminated string constant";
    curr_lineno++;
    BEGIN(INITIAL);
    return (ERROR);
}

<STRING><<EOF>> {
    cool_yylval.error_msg = "EOF in string constant";
    BEGIN(INITIAL);
    yyrestart(yyin);
    return (ERROR);
}

<STRING>\" {
    std::string input(yytext, yyleng), output;
    input = input.substr(1, yyleng-2);
    if (input.find('\0') != std::string::npos) {
        cool_yylval.error_msg = "String contains null character";
        BEGIN(INITIAL);
        return (ERROR);
    }
    std::string::size_type pos = 0, pos_next;
    while(pos_next = input.find('\\', pos) != std::string::npos) {
        output += input.substr(pos, pos_next);
        switch(input[pos_next+1]) {
            case 'b':
                output += '\b';
                break;
            case 't':
                output += '\t';
                break;
            case 'n':
                output += '\n';
                break;
            case 'f':
                output += '\f';
                break;
            default: 
                output += input[pos_next+1];
        }
        pos = pos_next + 2;
    }
    if (output.length() >= MAX_STR_CONST) {
        cool_yylval.error_msg = "String constant too long";
        BEGIN(INITIAL);
        return (ERROR);
    }
    cool_yylval.symbol = stringtable.add_string((char*)(output.c_str()));
    BEGIN(INITIAL);
    return (STR_CONST);
}

 /*
  *  Nested comments
  */

<NESTED_COMMENT>"(*" {
    nested_comment_layer++;
}

<NESTED_COMMENT>"*)" {
    nested_comment_layer--;
    if (nested_comment_layer == 0) {
        BEGIN(INITIAL);
    }
}

<NESTED_COMMENT>[^\n(*)]+ {}

<NESTED_COMMENT>[(*)]+ {}

<NESTED_COMMENT>\n {
    curr_lineno++;
}

<NESTED_COMMENT><<EOF>> {
    cool_yylval.error_msg = "EOF in comment";
    BEGIN(INITIAL);
    yyrestart(yyin);
    return (ERROR);
}

%%
