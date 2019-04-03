/* The MIT License (MIT)
 *
 * Copyright (c) 2019 Alexander Samoilov
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 */

/*
 *  spef.l: An SPEF scanner.
 *
 * $Id: SPEFlex.l,v 1.1.4.2 2003/02/04 16:58:39 ls Exp $
 *
 */

%{

#include <cstring>
#include <cctype>
#include <SPEFTokens.h>
#include "design_interface/spef_lexer.h"
#include "design_interface/flexlib.h"
#include "design_interface/parser.h"
#include "design_interface/parser_app.h"

extern int SPEF_lineno;
static void unescape();

#define YY_USER_INIT \
do {                 \
  SPEF_lineno = 1;    \
} while( 0 )

%}
/* Definitions */

white           [\t ]
eoln            "\r\n"|\r|\n

alpha           [a-zA-Z]
digit           [0-9]
exp_char        [eE]

pos_integer     {digit}+ 
pos_decimal     {digit}+\.{digit}*
pos_fraction    \.{digit}+

pos_radix       {pos_integer}|{pos_decimal}|{pos_fraction}
pos_exp         {pos_radix}{exp_char}{integer}
pos_float       {pos_decimal}|{pos_fraction}|{pos_exp}
pos_number      {pos_integer}|{pos_float}

sign            \+|\-
integer         {sign}?{pos_integer}
decimal         {sign}?{pos_decimal}
fraction        {sign}?{pos_fraction}

radix           {integer}|{decimal}|{fraction}
exp             {radix}{exp_char}{integer}
float           {decimal}|{fraction}|{exp}
number          {integer}|{float}

pf_triplet      {pos_fraction}":"{pos_fraction}":"{pos_fraction}
pv_triplet      {float}":"{float}":"{float}

special_char    [!#$%&`()*+,\-./:;<=>?@\[\\\]^'{|}~]
to_unescape     [^ \t\r\n]*\\[[:alnum:]_]

identifier      (\\({special_char}|\")|{alpha}|{digit}|\_)+
qstring         \"({special_char}|{alpha}|{digit}|{white}|\_)*\"
index           \*{pos_integer}

name            {qstring}|{identifier}

direction       [IBO]
time_unit       "NS"|"PS"
cap_unit        "PF"|"FF"
res_unit        "OHM"|"KOHM"
induc_unit      "HENRY"|"MH"|"UH"

/*************************/

hchar                   [./:|]
pb_delim                [[{(<.:]
sb_delim                []})>]

bit_identifier          {identifier}({pb_delim}{digit}+{sb_delim}?)?
path_or_pref            {identifier}({hchar}{identifier})*
physical_ref            {name}({hchar}{name})*
path                    {hchar}?{bit_identifier}({hchar}{bit_identifier})*{hchar}?

path_pref_delim         ({index}|{path}|{physical_ref}){hchar}({index}|{bit_identifier}|{name})

%p 6000

%option    never-interactive
%option    noyywrap
/*%option    yyclass="SpefLexer"*/
%option    prefix="SPEF_"

%x COMMENT
%%
                        int state;

"//".*                  /* Single-line comment. */

 /* Multi-line comment.  Taken from flex manual. */
"/*"                    { state = YY_START; BEGIN(COMMENT); }
<COMMENT>[^*\r\n]*      /* eat anything that's not a '*' */
<COMMENT>"*"+[^*/\r\n]* /* eat up '*'s not followed by '/'s */
<COMMENT>"*"+"/"        BEGIN(state);

{white}+                {}

<*>{eoln}               { if (++SPEF_lineno % 128 == 0
                              && db::ParserApp::progress_report != 0)
                            db::ParserApp::progress_report();
                        }

\*SPEF                 return TK_spef;
\*DESIGN               return TK_design;
\*DATE                 return TK_date;
\*VENDOR               return TK_vendor;
\*PROGRAM              return TK_program;
\*VERSION              return TK_version;
\*DESIGN_FLOW          return TK_design_flow;
\*DIVIDER              return TK_divider;
\*DELIMITER            return TK_delimiter;
\*BUS_DELIMITER        return TK_bus_delimiter;

\*T_UNIT               return TK_t_unit;
\*C_UNIT               return TK_c_unit;
\*R_UNIT               return TK_r_unit;
\*L_UNIT               return TK_l_unit;

\*NAME_MAP             return TK_name_map;
\*POWER_NETS           return TK_power_nets;
\*GROUND_NETS          return TK_ground_nets;

\*PORTS                return TK_ports;
\*PHYSICAL_PORTS       return TK_physical_ports;

\*C                    return TK_c;
\*L                    return TK_l;
\*S                    return TK_s;
\*D                    return TK_d;

\*DEFINE               return TK_define;
\*PDEFINE              return TK_pdefine;

\*D_NET                return TK_d_net;
\*R_NET                return TK_r_net;
\*D_PNET               return TK_d_pnet;
\*R_PNET               return TK_r_pnet;

\*END                  return TK_end;

\*V                    return TK_v;

\*CONN                 return TK_conn;
\*CAP                  return TK_cap;
\*RES                  return TK_res;
\*INDUC                return TK_induc;

\*P                    return TK_p;
\*I                    return TK_i;
\*N                    return TK_n;

\*DRIVER               return TK_driver;
\*CELL                 return TK_cell;
\*C2_R1_C1             return TK_c2_r1_c1;
\*LOADS                return TK_loads;

\*RC                   return TK_rc;
\*Q                    return TK_q;
\*K                    return TK_k;

 /* {to_unescape}          { unescape(); REJECT; } */

\.                     return DOT;
\/                     return SLASH;
\|                     return BAR;
\(                     return LPAREN;
\)                     return RPAREN;
\[                     return LBRACKET;
\]                     return RBRACKET;
\{                     return LBRACE;
\}                     return RBRACE;
\<                     return LANGEL;
\>                     return RANGEL;
\:                     return COLON;

{pos_integer}          return POS_INTEGER;
{pos_fraction}         return POS_FRACTION;
{pos_float}            return POS_FLOAT;
{float}                return FLOAT;
{number}               return NUMBER;

{pf_triplet}           return PF_TRIPLET;
{pv_triplet}           return PV_TRIPLET;

{direction}            return DIRECTION;
{time_unit}            return TIME_UNIT;
{cap_unit}             return CAP_UNIT;
{res_unit}             return RES_UNIT;
{induc_unit}           return INDUC_UNIT;

{index}                return INDEX;
{identifier}           return IDENTIFIER;
{qstring}              return QSTRING;
{bit_identifier}       return BIT_IDENTIFIER;
{physical_ref}         return PHYSICAL_REF;
{path}                 return PATH;

{path_pref_delim}      return PATH_PREF_DELIM;

<*>[^ \t\r\n]+         lexerror(SPEF_lineno, yytext, "unexpected");

%%

int SPEF_lineno = 1;

/* unescape(): removes slash before non-special characters in yytext.
   Updates yyleng accordingly.  */
static void unescape()
{
  static const char special[] = "!#$%`()*+,-./:;<=>?@[\\]^'{|}~\"";
  char *p = yytext, *q = 0;
  size_t shift = 0;
  
  while (1) {
    while (*p != 0 && *p != '\\')
      ++p;

    if (*p == 0)
      break;

    if (strchr(special, *++p) == 0) { /* Char after slash is not special. */
      if (q != 0)
        memmove(q - shift, q, p - 1 - q);
      
      q = p;
      ++shift;
    }

    ++p;

    /* Here p points two characters after the slash.  Note, however,
       that there is no undefined behavior here, since slash never is
       the last character in yytext (see {to_unescape} definition
       above).  */
  }

  if (q != 0) {
    memmove(q - shift, q, p + 1 - q);
    yyleng -= shift;
  }
}
