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
 *  lef.l: An LEF scanner.
 *
 * $Id: LEFlex.l,v 1.1.4.2 2003/02/04 16:58:38 ls Exp $
 *
 */

%{

#include <string.h>
#include <ctype.h>
#include <LEFTokens.h>
#include "design_interface/lef_lexer.h"
#include "design_interface/flexlib.h"
#include "design_interface/parser.h"
#include "design_interface/parser_app.h"

extern int LEF_lineno;
#define YY_USER_INIT \
do {                 \
  LEF_lineno = 1;    \
} while( 0 )

%}
/* Definitions */

white       [\t\f ]
eoln        "\r\n"|\r|\n
letter      [a-zA-Z]
eletter     [a-zA-Z_/=|\[\]\(\)\<\>\.!\{\}\-\$\:\\]
digit       [0-9]
num_int     {digit}+
sign        \+|\-
pnnum_int   {sign}?{digit}+ 
quote       \'\'
chr     [^']
/* chr_lit      \'(({chr})|({quote}))*\' */

nonquote    [^"]
quoted_str  \"({nonquote})*\"
comma       \,
semcol      \;
colon       \:
dot     \.
ord_br1     \(
ord_br2     \)
sq_br1      \[
sq_br2      \]
rel_op      \=|\>|\<|\>\=|\<\=|\<\>
plus            \+
minus           \-
star            \*
asgn        =
gletter   {eletter}|\*|#

%p 6000

%option    never-interactive
%option    noyywrap
/*%option    yyclass="LefLexer"*/
%option    prefix="LEF_"
%%

#.*                 {  }

{white}+            {  }            
{eoln}  { if (++LEF_lineno % 128 == 0 && db::ParserApp::progress_report != 0)
            db::ParserApp::progress_report(); }



";"             return SEMI;
{plus}          return PLUS;
{minus}         return MINUS;
{star}          return STAR;
"/"             return DIV;
"="             return ASSIGN;

"("             return LPAREN;
")"             return RPAREN;



ABUTMENT              return TK_abutment               ;
ACTIVE                return TK_active                 ;
ANALOG                return TK_analog                 ;
ANTENNAAREAFACTOR     return TK_antennaareafactor      ;
ANTENNALENGTHFACTOR   return TK_antennalengthfactor    ;
ANTENNAMETALAREA      return TK_antennametalarea       ;
ANTENNAMETALLENGTH    return TK_antennametallength     ;
ANTENNASIZE           return TK_antennasize            ;
ANYEDGE               return TK_anyedge                ;
ARRAY                 return TK_array                  ;
BEGINEXT              return TK_beginext               ;
BLOCK                 return TK_block                  ;
BOTTOMLEFT            return TK_bottomleft             ;
BOTTOMRIGHT           return TK_bottomright            ;
BUFFER                return TK_buffer                 ;
BUSBITCHARS           return TK_busbitchars            ;
BY                    return TK_by                     ;
CANNOTOCCUPY          return TK_cannotoccupy           ;
CANPLACE              return TK_canplace               ;
CAPACITANCE           return TK_capacitance            ;
CAPMULTIPLIER         return TK_capmultiplier          ;
CLASS                 return TK_class                  ;
CLOCK                 return TK_clock                  ;
CLOCKTYPE             return TK_clocktype              ;
COMPONENTPIN          return TK_componentpin           ;
CORE                  return TK_core                   ;
CORNER                return TK_corner                 ;
COVER                 return TK_cover                  ;
CORRECTIONFACTOR      return TK_correctionfactor       ;
CORRECTIONTABLE       return TK_correctiontable        ;
CPERSQDIST            return TK_cpersqdist             ;
CURRENT               return TK_current                ;
CURRENTDEN            return TK_currentden             ;
CURRENTSOURCE         return TK_currentsource          ;
CUT                   return TK_cut                    ;
DATA                  return TK_data                   ;
DATABASE              return TK_database               ;
DEFAULT               return TK_default                ;
DEFAULTCAP            return TK_defaultcap             ;
DELAY                 return TK_delay                  ;
DESIGN                return TK_design                 ;
DIELECTRIC            return TK_dielectric             ;
DIRECTION             return TK_direction              ;
DIVIDERCHAR           return TK_dividerchar            ;
DO                    return TK_do                     ;
E                     return TK_e                      ;
EEQ                   return TK_eeq                    ;
EDGECAPACITANCE       return TK_edgecapacitance        ;
EDGERATE              return TK_edgerate               ;
EDGERATESCALEFACTOR   return TK_edgeratescalefactor    ;
EDGERATETHRESHOLD1    return TK_edgeratethreshold1     ;
EDGERATETHRESHOLD2    return TK_edgeratethreshold2     ;
END                   return TK_end                    ;
ENDCAP                return TK_endcap                 ;
ENDEXT                return TK_endext                 ;
EXTENSION             return TK_extension              ;
FALL                  return TK_fall                   ;
FALLRS                return TK_fallrs                 ;
FALLCS                return TK_fallcs                 ;
FALLSATCUR            return TK_fallsatcur             ;
FALLSATT1             return TK_fallsatt1              ;
FALLSLEWLIMIT         return TK_fallslewlimit          ;
FALLT0                return TK_fallt0                 ;
FALLTHRESH            return TK_fallthresh             ;
FALLVOLTAGETHRESHOLD  return TK_fallvoltagethreshold   ;
FE                    return TK_fe                     ;
FEEDTHRU              return TK_feedthru               ;
FLOORPLAN             return TK_floorplan              ;
FN                    return TK_fn                     ;
FOREIGN               return TK_foreign                ;
FREQUENCY             return TK_frequency              ;
FROMPIN               return TK_frompin                ;
FS                    return TK_fs                     ;
FUNCTION              return TK_function               ;
FW                    return TK_fw                     ;
GCELLGRID             return TK_gcellgrid              ;
GENERATE              return TK_generate               ;
GROUND                return TK_ground                 ;
HEIGHT                return TK_height                 ;
HOLD                  return TK_hold                   ;
HORIZONTAL            return TK_horizontal             ;
INOUT                 return TK_inout                  ;
INOUTPINANTENNASIZE   return TK_inoutpinantennasize    ;
INPUT                 return TK_input                  ;
INPUTNOISEMARGIN      return TK_inputnoisemargin       ;
INPUTPINANTENNASIZE   return TK_inputpinantennasize    ;
INTEGER               return TK_integer                ;
INTRINSIC             return TK_intrinsic              ;
INVERT                return TK_invert                 ;
INVERTER              return TK_inverter               ;
IRDROP                return TK_irdrop                 ;
ITERATE               return TK_iterate                ;
IV_TABLES             return TK_iv_tables              ;
LAYER                 return TK_layer                  ;
LEAKAGE               return TK_leakage                ;
LEQ                   return TK_leq                    ;
LIBRARY               return TK_library                ;
LENGTHTHRESHOLD       return TK_lengththreshold        ;
MACRO                 return TK_macro                  ;
MASTERSLICE           return TK_masterslice            ;
MAXDELAY              return TK_maxdelay               ;
MAXLOAD               return TK_maxload                ;
MEGAHERTZ             return TK_megahertz              ;
METALOVERHANG         return TK_metaloverhang          ;
MICRONS               return TK_microns                ;
MILLIAMPS             return TK_milliamps              ;
MILLIWATTS            return TK_milliwatts             ;
MINFEATURE            return TK_minfeature             ;
MINPINS               return TK_minpins                ;
MUSTJOIN              return TK_mustjoin               ;
N                     return TK_n                      ;
NAMEMAPSTRING         return TK_namemapstring          ;
NAMESCASESENSITIVE    return TK_namescasesensitive     ;
NANOSECONDS           return TK_nanoseconds            ;
NEGEDGE               return TK_negedge                ;
NOISETABLE            return TK_noisetable             ;
NONDEFAULTRULE        return TK_nondefaultrule         ;
NONINVERT             return TK_noninvert              ;
NONUNATE              return TK_nonunate               ;
NOWIREEXTENSIONATPIN  return TK_nowireextensionatpin   ;
OBS                   return TK_obs                    ;
OFF                   return TK_off                    ;
OFFSET                return TK_offset                 ;
OHMS                  return TK_ohms                   ;
ON                    return TK_on                     ;
ORIGIN                return TK_origin                 ;
OUTPUT                return TK_output                 ;
OUTPUTNOISEMARGIN     return TK_outputnoisemargin      ;
OUTPUTRESISTANCE      return TK_outputresistance       ;
OUTPUTPINANTENNASIZE  return TK_outputpinantennasize   ;
OVERHANG              return TK_overhang               ;
OVERLAP               return TK_overlap                ;
PAD                   return TK_pad                    ;
PATH                  return TK_path                   ;
PICOFARADS            return TK_picofarads             ;
PIN                   return TK_pin                    ;
PITCH                 return TK_pitch                  ;
POLYGON               return TK_polygon                ;
PORT                  return TK_port                   ;
POSEDGE               return TK_posedge                ;
POST                  return TK_post                   ;
POWER                 return TK_power                  ;
PRE                   return TK_pre                    ;
PROPERTY              return TK_property               ;
PROPERTYDEFINITIONS   return TK_propertydefinitions    ;
PULLDOWNRES           return TK_pulldownres            ;
PWL                   return TK_pwl                    ;
R90                   return TK_r90                    ;
RANGE                 return TK_range                  ;
REAL                  return TK_real                   ;
RECT                  return TK_rect                   ;
RESISTANCE            return TK_resistance             ;
RESISTIVE             return TK_resistive              ;
RING                  return TK_ring                   ;
RISE                  return TK_rise                   ;
RISECS                return TK_risecs                 ;
RISERS                return TK_risers                 ;
RISESATCUR            return TK_risesatcur             ;
RISESATT1             return TK_risesatt1              ;
RISESLEWLIMIT         return TK_riseslewlimit          ;
RISET0                return TK_riset0                 ;
RISETHRESH            return TK_risethresh             ;
RISEVOLTAGETHRESHOLD  return TK_risevoltagethreshold   ;
ROUTING               return TK_routing                ;
RPERSQ                return TK_rpersq                 ;
S                     return TK_s                      ;
SAMENET               return TK_samenet                ;
SDFCOND               return TK_sdfcond                ;
SDFCONDEND            return TK_sdfcondend             ;
SDFCONDSTART          return TK_sdfcondstart           ;
SETUP                 return TK_setup                  ;
SETUPTIME             return TK_setuptime              ;
SHAPE                 return TK_shape                  ;
SHRINKAGE             return TK_shrinkage              ;
SIGNAL                return TK_signal                 ;
SITE                  return TK_site                   ;
SIZE                  return TK_size                   ;
SKIP                  return TK_skip                   ;
SOURCE                return TK_source                 ;
SPACER                return TK_spacer                 ;
SPACING               return TK_spacing                ;
STACK                 return TK_stack                  ;
START                 return TK_start                  ;
STEP                  return TK_step                   ;
STOP                  return TK_stop                   ;
STRING                return TK_string                 ;
STRUCTURE             return TK_structure              ;
SYMMETRY              return TK_symmetry               ;
TABLE                 return TK_table                  ;
TABLEAXIS             return TK_tableaxis              ;
TABLEENTRIES          return TK_tableentries           ;
TABLEDIMENSION        return TK_tabledimension         ;
TAPERRULE             return TK_taperrule              ;
THICKNESS             return TK_thickness              ;
TIEHIGH               return TK_tiehigh                ;
TIELOW                return TK_tielow                 ;
TIEOFF                return TK_tieoff                 ;
TIEOFFR               return TK_tieoffr                ;
TIME                  return TK_time                   ;
TIMING                return TK_timing                 ;
TO                    return TK_to                     ;
TOPIN                 return TK_topin                  ;
TOPLEFT               return TK_topleft                ;
TOPOFSTACKONLY        return TK_topofstackonly         ;
TOPRIGHT              return TK_topright               ;
TRACKS                return TK_tracks                 ;
TRANSITION            return TK_transition             ;
TRANSITIONTIME        return TK_transitiontime         ;
TRISTATE              return TK_tristate               ;
TYPE                  return TK_type                   ;
UNATENESS             return TK_unateness              ;
UNITS                 return TK_units                  ;
UNIVERSALNOISEMARGIN  return TK_universalnoisemargin   ;
USE                   return TK_use                    ;
USELENGTHTHRESHOLD    return TK_uselengththreshold     ;
USER                  return TK_user                   ;
VARIABLE              return TK_variable               ;
VERSION               return TK_version                ;
VERTICAL              return TK_vertical               ;
VHI                   return TK_vhi                    ;
VIA                   return TK_via                    ;
VIARULE               return TK_viarule                ;
VICTIMLENGTH          return TK_victimlength           ;
VICTIMNOISE           return TK_victimnoise            ;
VIRTUAL               return TK_virtual                ;
VLO                   return TK_vlo                    ;
VOLTAGE               return TK_voltage                ;
VOLTS                 return TK_volts                  ;
W                     return TK_w                      ;
WIDTH                 return TK_width                  ;
WIRECAP               return TK_wirecap                ;
WIREEXTENSION         return TK_wireextension          ;
X                     return TK_x                      ;
Y                     return TK_y                      ;


{quoted_str}                                               return QSTRING;

{pnnum_int}                                                return NUMBER;

{pnnum_int}[e|E]{sign}?{num_int}                           return NUMBER;

{sign}?{num_int}?{dot}{num_int}([e|E]{sign}?{num_int})?    return NUMBER;

({eletter}|{digit})({gletter}|{digit})*                    return T_STRING;

. lexerror(LEF_lineno, yytext, "unexpected");

%%

int LEF_lineno = 1;

