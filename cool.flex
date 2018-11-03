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
long int open_brace = 0;


%}

%option yylineno
%option noyywrap


%x COMMENT COMMENT2 STRING
/*
 * Define names for regular expressions here.
 */

DARROW          =>


TYPEID  [A-Z][A-Za-z0-9_]*

INT_CONST [0-9]+

STR_CONST \"[^\"]*\"

OBJECTID [a-z][A-Za-z0-9_]*

ERROR  _

PUNC [;:(){},.@<=\-+*/~]

CLOSE_COMM "*)"
OPEN_COMM  "(*"
%%

 /*
  *  Nested comments
  */


 /*
  *  The multiple-character operators.
  */


<INITIAL>-- {BEGIN(COMMENT2);}
<COMMENT2>[^\n] {}
<COMMENT2>"\n" {BEGIN(INITIAL);}


 /*
  *  возможны траблы
  */
<COMMENT>[^*)(]* {}
<COMMENT>"*"[^)] {}

<COMMENT>"("[^*] {}
<COMMENT>[^*]*")" {}


<INITIAL,COMMENT>{OPEN_COMM} {
    open_brace++;
    BEGIN(COMMENT);
}

<COMMENT>{CLOSE_COMM} {
    open_brace--;
    if(open_brace == 0)
    {
        BEGIN(INITIAL);
    }
    else
    {

    }
}


<COMMENT><<EOF>> {
     yylval.error_msg = "EOF in comment";
     BEGIN(INITIAL);
    return (ERROR);
}

<INITIAL>{CLOSE_COMM} { yylval.error_msg ="Unmatched *)"; return (ERROR); }


<INITIAL>\" {
   string_buf_ptr = string_buf;
   BEGIN(STRING);
}

<STRING>\n {yylval.error_msg ="Unterminated string constant"; BEGIN(INITIAL);  return (ERROR);} 


<STRING>\\[\"ntfb\\] {
    char tmpstr[1];

    switch(yytext[1])
    {
        case 'n':
            tmpstr[0] = '\n';
            break;

        case 't':
            tmpstr[0] = '\t';
            break;

        case 'f':
            tmpstr[0] = '\f';
            break;

        case 'b':
            tmpstr[0] = '\b';
            break;
        case '\"':
            tmpstr[0] = '\"';
            break;
        case '\\':
            tmpstr[0] = '\\';
            break;

        default:
            printf("sosi\n");
    }
    strncpy(string_buf_ptr, tmpstr, 1);
    string_buf_ptr++;
}

<STRING>\\[^\"ntfb\\] {

    char tmpstr[1];
    tmpstr[0] = yytext[1];
    strncpy(string_buf_ptr, tmpstr, 1);
    string_buf_ptr++;
}

<STRING>\" {
    *string_buf_ptr = '\0';
    cool_yylval.symbol = inttable.add_string(string_buf); 
    BEGIN(INITIAL);
    return (STR_CONST);
}

<STRING><<EOF>> {
      yylval.error_msg = "EOF in string constant";
        BEGIN(INITIAL);
          return ERROR;
}

<STRING>[^"\"] { 
    int len = strlen(yytext);
    strncpy(string_buf_ptr, yytext, len);
    string_buf_ptr+=len;
}




<INITIAL>[ ]+ {}
<INITIAL>[ \t\r\f\v]+ {}
<INITIAL>{DARROW} {return (DARROW); }
<INITIAL><= {return (LE); }
<INITIAL><- {return (ASSIGN); }



<INITIAL>(?-i:t)(?i:rue)     {    curr_lineno = yylineno; cool_yylval.boolean = true;  return (BOOL_CONST);}
<INITIAL>(?-i:f)(?i:alse) {curr_lineno = yylineno;  cool_yylval.boolean = false;  return (BOOL_CONST);  } 

<INITIAL>(?i:class)    {curr_lineno = yylineno;  return (CLASS); }
<INITIAL>(?i:else)   {curr_lineno = yylineno;  return (ELSE);  }
<INITIAL>(?i:fi)      {curr_lineno = yylineno;  return (FI);  }
<INITIAL>(?i:if)   { curr_lineno = yylineno;  return (IF); }
<INITIAL>(?i:in)       {curr_lineno = yylineno;  return (IN);  }
<INITIAL>(?i:inherits)   { curr_lineno = yylineno;  return (INHERITS); }
<INITIAL>(?i:let)       { curr_lineno = yylineno;  return (LET); }
<INITIAL>(?i:loop)    {curr_lineno = yylineno;  return (LOOP);  }
<INITIAL>(?i:pool)   { curr_lineno = yylineno;  return (POOL); }
<INITIAL>(?i:then)   {curr_lineno = yylineno;  return (THEN);  }
<INITIAL>(?i:while)   {curr_lineno = yylineno;  return (WHILE);  }
<INITIAL>(?i:case)    {curr_lineno = yylineno;  return (CASE);  }
<INITIAL>(?i:esac)   { curr_lineno = yylineno;  return (ESAC); }
<INITIAL>(?i:of)      { curr_lineno = yylineno;  return (OF); }
<INITIAL>(?i:new)     { curr_lineno = yylineno;  return (NEW); }
<INITIAL>(?i:isvoid)  { curr_lineno = yylineno;  return (ISVOID); }
<INITIAL>(?i:not)  {curr_lineno = yylineno;  return (NOT);  }


<INITIAL>{PUNC} { return ((char)yytext[0]); }


  <INITIAL>{INT_CONST} { cool_yylval.symbol = inttable.add_string(yytext); return (INT_CONST);}
  <INITIAL>{OBJECTID}  { cool_yylval.symbol = inttable.add_string(yytext); return (OBJECTID);}
  <INITIAL>{TYPEID}    {cool_yylval.symbol = inttable.add_string(yytext); return (TYPEID);}


<INITIAL>.          { yylval.error_msg = yytext; return (ERROR); }


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
