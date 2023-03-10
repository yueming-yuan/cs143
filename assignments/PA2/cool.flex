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

// char string_buf[MAX_STR_CONST]; /* to assemble string constants */
// char *string_buf_ptr;

std::string string_buf;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

int cmt = 0;
%}

%option noyywrap

/*
 * Define names for regular expressions here.
 */


INTEGER         [0-9]+
TID             [A-Z][A-Za-z0-9_]*
OID             [a-z][A-Za-z0-9_]*

DARROW          =>
ASSIGN          <-
LE              <=

%x COMMENT
%x STRING   

%%

 /*
  *  Nested comments
  */

--[^\n]*            
"(*"            BEGIN(COMMENT);

<COMMENT>"(*"           cmt ++;
<COMMENT>[^(*\n]+    
<COMMENT>(\*[^*)\n]*)|(\([^*\n]*)

<COMMENT>"*)"           {   if(!cmt) 
                                BEGIN(INITIAL);
                            else
                                cmt --;     }

<COMMENT><<EOF>>        {   BEGIN(INITIAL);
                            yylval.error_msg = "EOF in comment";
                            return ERROR;   }

"*)"                    {   
                            yylval.error_msg = "Unmatched *)";
                            return ERROR;   }

 /*
  *  The multiple-character operators.
  */
{DARROW}		return DARROW; 
{ASSIGN}        return ASSIGN;
{LE}            return LE;

[+\-\*/=,.@~<(){}:;]         return (int)*yytext;


 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
(?i:class)      return CLASS;
(?i:if)         return IF;
(?i:else)       return ELSE;
(?i:fi)         return FI;
(?i:in)         return IN;
(?i:inherits)   return INHERITS;
(?i:let)        return LET;
(?i:loop)       return LOOP;
(?i:pool)       return POOL;
(?i:then)       return THEN;
(?i:while)      return WHILE;
(?i:case)       return CASE;
(?i:esac)       return ESAC;
(?i:of)         return OF;
(?i:new)        return NEW;
(?i:not)        return NOT;
(?i:isvoid)     return ISVOID;

t(?i:rue)       { cool_yylval.boolean = 1;
                  return BOOL_CONST; }

f(?i:alse)      { cool_yylval.boolean = 0;
                  return BOOL_CONST; }

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */

[ \b\t\f\r\v]+      

<INITIAL,COMMENT>\n              curr_lineno ++;


{INTEGER}       { yylval.symbol = inttable.add_string(yytext);
                  return INT_CONST; }

{TID}           { cool_yylval.symbol = stringtable.add_string(yytext);
                  return TYPEID; }

{OID}           { cool_yylval.symbol = stringtable.add_string(yytext);
                  return OBJECTID; }


\"                  {   string_buf.clear();
                        BEGIN(STRING);  }

<STRING>\"          {
                        BEGIN(INITIAL);
                        if(string_buf.length() >= MAX_STR_CONST) {
                            yylval.error_msg = "String constant too long";
                            return ERROR;
                        }
                        
                        yylval.symbol = stringtable.add_string((char*)string_buf.c_str());
                        return STR_CONST;
                    }

<STRING>[^\"\\\0\n]*  string_buf += yytext;

<STRING>\n          {     
                        curr_lineno ++;  
                        BEGIN(INITIAL);
                        yylval.error_msg = "Unterminated string constant";
                        return ERROR;
                    }

<STRING><<EOF>>     {
                        BEGIN(INITIAL);  
                        yylval.error_msg = "EOF in string constant";
                        return ERROR;
                    }

<STRING>{
    \\n             string_buf += '\n';
    \\t             string_buf += '\t';
    \\b             string_buf += '\b';
    \\f             string_buf += '\f';

    \\\n            {
                        curr_lineno ++;
                        string_buf += '\n';
                    }

    \0[^"\n]*/\n    |
    \0[^"\n]*\"     {
                        BEGIN(INITIAL);
                        yylval.error_msg = "String contains null character.";
                        return ERROR;
                    }

    \\\0[^"\n]*/\n  |
    \\\0[^"\n]*\"   {
                        BEGIN(INITIAL);
                        yylval.error_msg = "String contains escaped null character.";
                        return ERROR;
                    }

    \\[^\n]         string_buf += yytext[1];
}

.                   {
                        yylval.error_msg = yytext;
                        return ERROR;
                    }

%%
