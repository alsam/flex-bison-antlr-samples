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
 *  def.l: An DEF scanner.
 *
 * $Id: DEFlex.l,v 1.1.4.7 2003/04/03 09:38:15 spike Exp $
 *
 */

%{

#include <cstring>
#include <cctype>
#include <DEFTokens.h>
#include "design_interface/def_lexer.h"
#include "design_interface/flexlib.h"
#include "design_interface/parser.h"
#include "design_interface/parser_app.h"

extern int DEF_lineno;
#define YY_USER_INIT \
do {                 \
  DEF_lineno = 1;    \
} while( 0 )

%}
/* Definitions */

white       [\t\f ]
eoln        "\r\n"|\r|\n
letter      [a-zA-Z]
eletter     [a-zA-Z_/=|\[\]\(\)\<\>\.!\{\}\-\$\:\\@]
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
gletter   {eletter}|\*|%

%p 6000

%option    never-interactive
%option    noyywrap
/*%option    yyclass="DefLexer"*/
%option    prefix="DEF_"
%%

#.*                 {  }

{white}+            {  }
{eoln}  { if (++DEF_lineno % 128 == 0 && db::ParserApp::progress_report != 0)
            db::ParserApp::progress_report(); }


";"             return SEMI;
{plus}          return PLUS;
{minus}         return MINUS;
{star}          return STAR;

"("             return LPAREN;
")"             return RPAREN;


VERSION               return TK_version                ;
NAMESCASESENSITIVE    return TK_namescasesensitive     ;
NAMEMAPSTRING         return TK_namemapstring          ;
HISTORY               return TK_history                ;
DESIGN                return TK_design                 ;
VIAS                  return TK_vias                   ;
TECHNOLOGY            return TK_tech                   ;
UNITS                 return TK_units                  ;
BUSBITCHARS           return TK_busbitchars            ;
DIVIDERCHAR           return TK_dividerchar            ;
DISTANCE              return TK_distance               ;
MICRONS               return TK_microns                ;
RECT                  return TK_rect                   ;
REENTRANTPATHS        return TK_reentrantpaths         ;
SITE                  return TK_site                   ;
CANPLACE              return TK_canplace               ;
CANNOTOCCUPY          return TK_cannotoccupy           ;
DIEAREA               return TK_diearea                ;
PINS                  return TK_pins                   ;
DEFAULTCAP            return TK_defaultcap             ;
MINPINS               return TK_minpins                ;
WIRECAP               return TK_wirecap                ;
TRACKS                return TK_tracks                 ;
GCELLGRID             return TK_gcellgrid              ;
DO                    return TK_do                     ;
BY                    return TK_by                     ;
STEP                  return TK_step                   ;
LAYER                 return TK_layer                  ;
COMPONENTS            return TK_components             ;
GENERATE              return TK_generate               ;
SOURCE                return TK_source                 ;
WEIGHT                return TK_weight                 ;
FIXED                 return TK_fixed                  ;
COVER                 return TK_cover                  ;
PLACED                return TK_placed                 ;
UNPLACED              return TK_unplaced               ;
FOREIGN               return TK_foreign                ;
EEQMASTER             return TK_eeqmaster              ;
REGION                return TK_region                 ;
REGIONS               return TK_regions                ;
NETS                  return TK_nets                   ;
MUSTJOIN              return TK_mustjoin               ;
ORIGINAL              return TK_original               ;
USE                   return TK_use                    ;
STYLE                 return TK_style                  ;
PATTERN               return TK_pattern                ;
PATTERNNAME           return TK_patternname            ;
NONDEFAULTRULE        return TK_nondefaultrule         ;
ESTCAP                return TK_estcap                 ;
ROUTED                return TK_routed                 ;
ROW                   return TK_row                    ;
TAPER                 return TK_taper                  ;
TAPERRULE             return TK_taperrule              ;

NEW                   return TK_new                    ;
SHAPE                 return TK_shape                  ;
SPECIALNETS           return TK_snets                  ;
SPECIALNET            return TK_snet                   ;
WIDTH                 return TK_width                  ;
VOLTAGE               return TK_voltage                ;
SPACING               return TK_spacing                ;

ON                    return TK_on                     ;
OFF                   return TK_off                    ;

N                     return TK_n                      ;
S                     return TK_s                      ;
E                     return TK_e                      ;
W                     return TK_w                      ;
FN	                  return TK_fn		     ;
FS	                  return TK_fs         ;
FE	                  return TK_fe         ;
FW	                  return TK_fw         ;

GROUPS                return TK_groups                 ;
GROUP                 return TK_group                  ;
COMPONENT             return TK_component              ;
SOFT                  return TK_soft                   ;
MAXX                  return TK_maxx                   ;
MAXY                  return TK_maxy                   ;
MAXHALFPERIMETER      return TK_maxhalfperimeter       ;
CONSTRAINTS           return TK_constraints            ;
NET                   return TK_net	               ;
PATH                  return TK_path	               ;
SUM                   return TK_sum	               ;
DIFF                  return TK_diff	 	       ;
RISEMIN               return TK_risemin   	       ;
RISEMAX               return TK_risemax   	       ;
FALLMIN               return TK_fallmin   	       ;
FALLMAX               return TK_fallmax   	       ;
WIREDLOGIC            return TK_wiredlogic             ;
MAXDIST               return TK_maxdist                ;
ASSERTIONS            return TK_assertions             ;
END                   return TK_end                    ;
ARRAY                 return TK_array                  ;
FLOORPLAN             return TK_floorplan              ;

SCANCHAINS            return TK_scanchains  	       ;
START                 return TK_start       	       ;
FLOATING              return TK_floating    	       ;
ORDERED               return TK_ordered     	       ;
STOP                  return TK_stop        	       ;
IN                    return TK_in          	       ;
OUT                   return TK_out         	       ;

TIMINGDISABLES        return TK_timingdisables	       ;
IOTIMINGS             return TK_iotimings              ;
RISE                  return TK_rise         	       ;
FALL                  return TK_fall         	       ;
VARIABLE              return TK_variable     	       ;
SLEWRATE              return TK_slewrate     	       ;
CAPACITANCE           return TK_capacitance  	       ;
DRIVECELL             return TK_drivecell    	       ;
FROMPIN               return TK_frompin      	       ;
TOPIN                 return TK_topin                ;
PARALLEL              return TK_parallel    	       ;
PARTITIONS            return TK_partitions  	       ;
TURNOFF               return TK_turnoff     	       ;
FROMCLOCKPIN          return TK_fromclockpin           ;
FROMCOMPPIN           return TK_fromcomppin            ;
FROMIOPIN             return TK_fromiopin              ;
TOCLOCKPIN            return TK_toclockpin             ;
TOCOMPPIN             return TK_tocomppin              ;
TOIOPIN               return TK_toiopin                ;
SETUPRISE             return TK_setuprise              ;
SETUPFALL             return TK_setupfall              ;
HOLDRISE              return TK_holdrise               ;
HOLDFALL              return TK_holdfall               ;

VPIN                  return TK_vpin                   ;
SUBNET                return TK_subnet                 ;
XTALK                 return TK_xtalk                  ;
PIN                   return TK_pin                    ;
SYNTHESIZED           return TK_synthesized            ;

SPECIAL               return TK_special                ;
DIRECTION             return TK_direction              ;
RANGE                 return TK_range	                 ;

FLOORPLANCONSTRAINTS  return TK_fpc                    ;
HORIZONTAL            return TK_horizontal             ;
VERTICAL              return TK_vertical               ;
ALIGN                 return TK_align                  ;
MIN                   return TK_min                    ;
MAX                   return TK_max                    ;
EQUAL                 return TK_equal                  ;
BOTTOMLEFT            return TK_bottomleft             ;
TOPRIGHT              return TK_topright               ;
ROWS                  return TK_rows                   ;

INPUT                 return TK_input                  ;
INOUT                 return TK_inout                  ; 
OUTPUT                return TK_output                 ;
FEEDTHRU              return TK_feedthru               ;

SIGNAL                return TK_signal                 ;
POWER                 return TK_power                  ;
GROUND                return TK_ground                 ;
CLOCK                 return TK_clock                  ;
TIEOFF                return TK_tieoff                 ;
ANALOG                return TK_analog                 ;

RING                  return TK_ring                   ;
STRIPE                return TK_stripe                 ;
FOLLOWPIN             return TK_followpin              ;
IOWIRE                return TK_iowire                 ;
BLOCKWIRE             return TK_blockwire              ;
COREWIRE              return TK_corewire               ;
FILLWIRE              return TK_fillwire               ;
BLOCKAGEWIRE          return TK_blockagewire           ;

X                     return TK_x                      ;
Y                     return TK_y                      ;

PROPERTYDEFINITIONS   return TK_propertydefinitions    ;
PROPERTY              return TK_property               ;
STRING                return TK_string	               ;
REAL                  return TK_real                   ;
INTEGER               return TK_integer                ;
BEGINEXT              return TK_beginext               ;
ENDEXT                return TK_endext	               ;
PINPROPERTIES         return TK_pinproperties          ;
COMMONSCANPINS        return TK_commonscanpins         ;
COMPONENTPIN          return TK_componentpin           ;
SHIELD                return TK_shield                 ;
SHIELDNET             return TK_shieldnet              ;
NOSHIELD              return TK_noshield               ;

BLOCKAGES             return TK_blockages              ; /* 5.5 extension */
SLOTS                 return TK_slots                  ; /* ditto */
FILLS                 return TK_fills                  ; /* ditto */
PUSHDOWN              return TK_pushdown               ; /* ditto */
PLACEMENT             return TK_placement              ; /* ditto */

{quoted_str}                                               return QSTRING;

{pnnum_int}                                                return NUMBER;

{pnnum_int}[e|E]{sign}?{num_int}                           return NUMBER;

{sign}?{num_int}?{dot}{num_int}([e|E]{sign}?{num_int})?    return NUMBER;

({eletter}|{digit})({gletter}|{digit})*                    return T_STRING;


. // skip silently (for parsing HISTORY statement, otherwise may be lexerror(DEF_lineno, yytext, "unexpected");

%%

int DEF_lineno = 1;
