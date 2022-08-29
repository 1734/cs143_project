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

\" {
    BEGIN(STRING);
    yymore();
}

<STRING>[^\\\n\"]+ { yymore(); }

<STRING>\\[^\n] { yymore(); }

<STRING>\\\n {
    curr_lineno++;
    yymore();
}

<STRING>\n {
    cool_yylval.error_msg="Unterminated string constant";
    return (ERROR);
}

 /*
  *  Nested comments
  */


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

(?i:class) { return (CLASS); }
(?i:else) { return (ELSE); }
f(?i:alse) { cool_yylval.Boolean = 0; return (BOOL_CONST); }
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
t(?i:rue) { cool_yylval.Boolean = 1; return (BOOL_CONST); }

[0-9]+ {
    cool_yylval.symbol = inttable.add_string(cool_yylex);
    return (INT_CONST);
}

[a-z][a-zA-z0-9_]* {
    cool_yylval.symbol = idtable.add_string(cool_yylex);
    return (OBJECTID);
}

[A-Z][a-zA-z0-9_]* {
    cool_yylval.symbol = idtable.add_string(cool_yylex);
    return (TYPEID);
}

{WHITESPACE} {}



 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */


 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */


%%
