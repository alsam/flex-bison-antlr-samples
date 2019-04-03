// The MIT License (MIT)
//
// Copyright (c) 2019 Alexander Samoilov
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE

//
//  LEF grammar parser using PCCTS.
//
// $Id: LEFparse.g,v 1.1.4.13 2003/03/04 13:34:13 spike Exp $
//

#header <<
  #include "design_interface/db_support.h"
  #include "design_interface/dmsg.h"

  using db::DBSupport;

  #define PURIFY(r,s)
  #define PCCTS_PURIFY(r,s)
>>

<<

  typedef ANTLRCommonToken ANTLRToken;
  using db::Rat;
  using db::Unit;
  using db::Rect;
  using db::Pt;
  using db::ViaRule_Layer;
  using db::Symmetry;
  using db::StepPattern;
  using db::SitePattern;
  using db::TrackPattern;
  using db::GcellPattern;
  using db::Foreign;

>>


class LEFParse : public DBSupport
{

<<


  typedef std::pair<Unit,Unit> UnitRange;

  void checkEndConstruct(ANTLRTokenPtr i1, ANTLRTokenPtr i2,
                         ANTLRTokenPtr keyword = 0) {
    if (i1 == 0 || i2 == 0) return;
    if (strcmp( i1->getText(), i2->getText()) != 0) {
      db::err( "\nSemantic error: " );
      if (keyword != 0) {
        fprintf(stderr, "in rule '%s': ", keyword->getText());
      }
      fprintf(stderr, "identifiers: '%s' at line %d and '%s' at line %d "
                      "doesn\'t match\n",
              i1->getText(), i1->getLine(),
              i2->getText(), i2->getLine());
      init_line_info();
    }
  }

>>


#token TK_abutment
#token TK_active
#token TK_analog
#token TK_antennaareafactor
#token TK_antennalengthfactor
#token TK_antennametalarea
#token TK_antennametallength
#token TK_antennasize
#token TK_anyedge
#token TK_array
#token TK_beginext
#token TK_block
#token TK_bottomleft
#token TK_bottomright
#token TK_buffer
#token TK_busbitchars
#token TK_by
#token TK_cannotoccupy
#token TK_canplace
#token TK_capacitance
#token TK_capmultiplier
#token TK_class
#token TK_clock
#token TK_clocktype
#token TK_componentpin
#token TK_core
#token TK_corner
#token TK_cover
#token TK_correctionfactor
#token TK_correctiontable
#token TK_cpersqdist
#token TK_current
#token TK_currentden
#token TK_currentsource
#token TK_cut
#token TK_data
#token TK_database
#token TK_default
#token TK_defaultcap
#token TK_delay
#token TK_design
#token TK_dielectric
#token TK_direction
#token TK_dividerchar
#token TK_do
#token TK_e
#token TK_eeq
#token TK_edgecapacitance
#token TK_edgerate
#token TK_edgeratescalefactor
#token TK_edgeratethreshold1
#token TK_edgeratethreshold2
#token TK_end
#token TK_endcap
#token TK_endext
#token TK_extension
#token TK_fall
#token TK_fallrs
#token TK_fallcs
#token TK_fallsatcur
#token TK_fallsatt1
#token TK_fallslewlimit
#token TK_fallt0
#token TK_fallthresh
#token TK_fallvoltagethreshold
#token TK_fe
#token TK_feedthru
#token TK_floorplan
#token TK_fn
#token TK_foreign
#token TK_frequency
#token TK_frompin
#token TK_fs
#token TK_function
#token TK_fw
#token TK_gcellgrid
#token TK_generate
#token TK_ground
#token TK_height
#token TK_hold
#token TK_horizontal
#token TK_inout
#token TK_inoutpinantennasize
#token TK_input
#token TK_inputnoisemargin
#token TK_inputpinantennasize
#token TK_integer
#token TK_intrinsic
#token TK_invert
#token TK_inverter
#token TK_irdrop
#token TK_iterate
#token TK_iv_tables
#token TK_layer
#token TK_leakage
#token TK_leq
#token TK_library
#token TK_lengththreshold
#token TK_macro
#token TK_masterslice
#token TK_maxdelay
#token TK_maxload
#token TK_megahertz
#token TK_metaloverhang
#token TK_microns
#token TK_milliamps
#token TK_milliwatts
#token TK_minfeature
#token TK_minpins
#token TK_mustjoin
#token TK_n
#token TK_namemapstring
#token TK_namescasesensitive
#token TK_nanoseconds
#token TK_negedge
#token TK_noisetable
#token TK_nondefaultrule
#token TK_noninvert	
#token TK_nonunate
#token TK_nowireextensionatpin
#token TK_obs
#token TK_off
#token TK_offset
#token TK_ohms
#token TK_on
#token TK_origin
#token TK_output
#token TK_outputnoisemargin
#token TK_outputresistance
#token TK_outputpinantennasize
#token TK_overhang
#token TK_overlap
#token TK_pad
#token TK_path
#token TK_picofarads
#token TK_pin
#token TK_pitch
#token TK_polygon
#token TK_port
#token TK_posedge
#token TK_post
#token TK_power
#token TK_pre
#token TK_property
#token TK_propertydefinitions
#token TK_pulldownres
#token TK_pwl		
#token TK_r90
#token TK_range
#token TK_real
#token TK_rect
#token TK_resistance
#token TK_resistive
#token TK_ring
#token TK_rise
#token TK_risecs
#token TK_risers
#token TK_risesatcur
#token TK_risesatt1
#token TK_riseslewlimit
#token TK_riset0
#token TK_risethresh
#token TK_risevoltagethreshold
#token TK_routing
#token TK_rpersq
#token TK_s
#token TK_samenet
#token TK_sdfcond
#token TK_sdfcondend
#token TK_sdfcondstart
#token TK_setup
#token TK_setuptime
#token TK_shape
#token TK_shrinkage
#token TK_signal
#token TK_site
#token TK_size
#token TK_skip
#token TK_source
#token TK_spacer
#token TK_spacing
#token TK_stack
#token TK_start
#token TK_step
#token TK_stop
#token TK_string
#token TK_structure
#token TK_symmetry
#token TK_table	
#token TK_tableaxis
#token TK_tableentries
#token TK_tabledimension
#token TK_taperrule
#token TK_thickness
#token TK_tiehigh
#token TK_tielow
#token TK_tieoff
#token TK_tieoffr
#token TK_time	
#token TK_timing
#token TK_to
#token TK_topin
#token TK_topleft	
#token TK_topofstackonly
#token TK_topright
#token TK_tracks	
#token TK_transition
#token TK_transitiontime
#token TK_tristate
#token TK_type	
#token TK_unateness
#token TK_units
#token TK_universalnoisemargin
#token TK_use
#token TK_uselengththreshold
#token TK_user
#token TK_variable
#token TK_version	
#token TK_vertical
#token TK_vhi
#token TK_via	
#token TK_viarule
#token TK_victimlength
#token TK_victimnoise
#token TK_virtual	
#token TK_vlo	
#token TK_voltage
#token TK_volts
#token TK_w
#token TK_width
#token TK_wirecap
#token TK_wireextension
#token TK_x
#token TK_y


#token NONE
#token NIL
#token Eof "@"

#errclass IDENTIFIER {tstring}

start_translation:
  rules { end_library } Eof
;

version: TK_version NUMBER SEMI
      <<;>>
  ;

dividerchar: TK_dividerchar c:QSTRING SEMI
    <<lef_dividerchar_ = strip_qstring(c->getText());>>
  ;

busbitchars: TK_busbitchars c:QSTRING SEMI
    <<lef_busbitchars_ = strip_qstring(c->getText());>>
  ;

rules: ( rule )*
  ;

end_library: TK_end TK_library
  ;

rule: version | busbitchars | case_sensitivity | units_section
    | layer_rule | via | viarule | dividerchar
    | wireextension | msg_statement
    | spacing_rule | dielectric | minfeature | irdrop | site | macro | array
    | def_statement | nondefault_rule | prop_def_section
    | universalnoisemargin | edgeratethreshold1
    | edgeratescalefactor | edgeratethreshold2
    | noisetable | correctiontable | input_antenna
    | output_antenna | inout_antenna | extension
    | create_file_statement
    ;

case_sensitivity: TK_namescasesensitive
  ( TK_on
    <<lef_namescasesensitive_ = true;
      db::string_compare::set_case_sensitive();
    >>
  | TK_off
    <<lef_namescasesensitive_ = false;
      db::string_compare::set_case_insensitive();
    >>
  ) SEMI
  ;

wireextension: TK_nowireextensionatpin
  ( TK_on  <<nowireextension_ = true;  >>
  | TK_off <<nowireextension_ = false; >>
  ) SEMI
  ;

units_section:
  TK_units
  ( units_rule )*
  TK_end TK_units
    <<;>>
  ;

units_rule:
  <<int n; ANTLRTokenPtr i;>>
  ( TK_time        TK_nanoseconds  inumber>[n] SEMI
    <<units_.setNanoseconds(n);    >>
  | TK_capacitance TK_picofarads   inumber>[n] SEMI
    <<units_.setCapacitance(n);    >>
  | TK_resistance  TK_ohms         inumber>[n] SEMI
    <<units_.setResistance(n);     >>
  | TK_power       TK_milliwatts   inumber>[n] SEMI
    <<units_.setPower(n);          >>
  | TK_current     TK_milliamps    inumber>[n] SEMI
    <<units_.setCurrent(n);        >>
  | TK_voltage     TK_volts        inumber>[n] SEMI
    <<units_.setVoltage(n);        >>
  | TK_database tstring>[i]        inumber>[n] SEMI
    <<if (strlwr(i) != "microns") db::warn2(i,"only `database microns' supported");
     else units_.setLefMicrons(n);    >>
  | TK_frequency   TK_megahertz    inumber>[n] SEMI
    <<units_.setFrequency(n);      >>
  )
  ;

layer_rule:
  <<Layer *aLayer; ANTLRTokenPtr i1,i2;>>
  k:TK_layer tstring>[i1] <<aLayer = layers_.find_or_insert(text(i1));>>
	     (layer_option<[aLayer])*
    TK_end   tstring>[i2] <<checkEndConstruct(i1,i2,$k);>>
  ;

layer_option < [Layer *aLayer]:
  << Rat n; Unit d; UnitRange r; ANTLRTokenPtr i; >>
  ( TK_type layer_type<[$aLayer] SEMI
  | TK_pitch   unumber>[d] SEMI
    <<$aLayer->setPitch(d);>>
  | TK_offset  unumber>[d] SEMI
    <<$aLayer->setOffset(d);>>
  | TK_width   unumber>[d] SEMI
    <<$aLayer->setWidth(d);>>
  | TK_spacing unumber>[d]
    <<r = std::make_pair(0,0);>>
   { range>[r] }
   {TK_layer tstring>[i] } SEMI
    <<$aLayer->addSpacing(d,r);>>
  | TK_direction layer_direction<[$aLayer] SEMI
  | TK_resistance TK_rpersq 
    ( number>[n]
    | TK_pwl LPAREN res_points RPAREN <<n = Rat();>>
    ) SEMI
    <<$aLayer->setResistance(n);>>
  | TK_capacitance TK_cpersqdist
    ( number>[n]
    | TK_pwl LPAREN cap_points RPAREN <<n = Rat();>>
    ) SEMI
    <<$aLayer->setCapacitance(n);>>
  | TK_height        unumber>[d] SEMI
    <<$aLayer->setHeight(d);>>
  | TK_wireextension unumber>[d] SEMI
    <<$aLayer->setWireextension(d);>>
  | TK_thickness     unumber>[d] SEMI
    <<$aLayer->setThickness(d);>>
  | TK_shrinkage     unumber>[d] SEMI
    <<$aLayer->setShrinkage(d);>>
  | TK_capmultiplier number>[n] SEMI
    <<$aLayer->setCapmultiplier(n);>>
  | TK_edgecapacitance number>[n] SEMI
    <<$aLayer->setEdgeCapacitance(n);>>
  | TK_antennaareafactor number>[n] SEMI
    <<$aLayer->setAntennaAreaFactor(n);>>
  | TK_antennalengthfactor number>[n] SEMI
    <<$aLayer->setAntennaLengthFactor(n);>>
  | TK_currentden
    ( NUMBER
    | TK_pwl LPAREN current_density_pwl_list RPAREN
    | LPAREN NUMBER NUMBER RPAREN 
    ) SEMI
    <<;>>
  | TK_property layer_prop_list SEMI
    <<;>>
  | TK_accurrentdensity layer_table_type
    (ac_layer_table_opt_list|NUMBER) SEMI
  | TK_dccurrentdensity TK_average
    (dc_layer_table_opt_list|NUMBER) SEMI
  )
  ;

layer_table_type:
    TK_peak     <<;>>
  | TK_average  <<;>>
  | TK_rms      <<;>>
  ;

ac_layer_table_opt_list: ( ac_layer_table_opt )*
  ;

ac_layer_table_opt:
  TK_frequency NUMBER
    <<;>>
  number_list SEMI
    <<;>>
  | TK_tableentries NUMBER
    <<;>>
  number_list //SEMI // SEMI missed in Cadence LEF/DEF
    <<;>>
  | TK_width NUMBER
    <<;>>
  number_list SEMI
    <<;>>
  ;

dc_layer_table_opt_list: ( dc_layer_table_opt )*
  ;

dc_layer_table_opt:
  TK_cutarea NUMBER
    <<;>>
  number_list SEMI
    <<;>>
  | TK_tableentries NUMBER
    <<;>>
  number_list //SEMI // SEMI missed in Cadence LEF/DEF
    <<;>>
  | TK_width NUMBER
    <<;>>
  number_list SEMI
    <<;>>
  ;

number_list: ( NUMBER )*
  ;

layer_prop_list: ( layer_prop )+
  ;

layer_prop:
  <<ANTLRTokenPtr i,i1;>>
  tstring>[i] ( tstring>[i1] | QSTRING | NUMBER )
  ;

current_density_pwl_list : ( current_density_pwl )+
  ;

current_density_pwl: LPAREN NUMBER NUMBER RPAREN
    <<;>>
  ;

cap_points : ( cap_point )+
  ;

cap_point: LPAREN NUMBER NUMBER RPAREN
    <<;>>
  ;

res_points : ( res_point )+
  ;

res_point: LPAREN NUMBER NUMBER RPAREN
    <<;>>
  ;

layer_type < [Layer* aLayer] :
  ( TK_routing     <<aLayer->setType(Layer::ROUTING);         >>
  | TK_cut         <<aLayer->setType(Layer::CUT);             >>
  | TK_overlap     <<aLayer->setType(Layer::OVERLAP);         >>
  | TK_masterslice <<aLayer->setType(Layer::MASTERSLICE);     >>
  | TK_virtual     <<aLayer->setType(Layer::VIRTUAL);         >>
  )
  ;

layer_direction < [Layer* aLayer]:
  ( TK_horizontal  <<aLayer->setDirection(Layer::HORIZONTAL); >>
  | TK_vertical    <<aLayer->setDirection(Layer::VERTICAL);   >>
  )
  ;

via:
  <<std::pair<Via*,bool>  via_tie;  Via* aVia; ANTLRTokenPtr i1,i2;>>
  k:TK_via tstring>[i1] <<via_tie = vias_.find_or_insert2(text(i1));
                         aVia = via_tie.first;
                         if (!via_tie.second) { //
                           db::warn2(i1, "Via %s redefinition", text(i1));
                           aVia->destroy_geom();
                         } 
                         >>
            {TK_default <<aVia->setDefault();>>}
             (via_option<[aVia])*
    TK_end tstring>[i2] <<checkEndConstruct(i1,i2,$k);>>
  ;

via_option < [Via *aVia]:
  << Rat n; >>
  ( via_foreign[$aVia]
  | via_layer_rule[$aVia]
  | TK_resistance number>[n] SEMI
    <<$aVia->setResistance(n);>>
  | TK_property via_prop_list SEMI
  | TK_topofstackonly
    <<$aVia->setTopOfStack();>>
  )
  ;

via_prop_list: ( via_name_value_pair )+
  ;

via_name_value_pair:
  <<ANTLRTokenPtr i,i1; Rat n;>>
  tstring>[i] ( number>[n] | QSTRING | tstring>[i1] )
  ;

via_foreign < [Via *aVia]:
  <<Foreign *f = 0;>>
  foreign>[f] <<aVia->setForeign(f);>>
  ;

foreign > [Foreign *f]:
  <<ANTLRTokenPtr foreign_id; Pt p; int ori;>>
  start_foreign>[foreign_id] <<$f = new Foreign(text(foreign_id));>>
  { pt>[p] <<$f->setPoint(p);>>
   {orientation>[ori] <<$f->setOrient(ori);>>}
  | orientation>[ori] <<$f->setOrient(ori);>>
  } SEMI
  ;

start_foreign > [ANTLRTokenPtr foreign_id]:
  TK_foreign tstring>[$foreign_id]
  ;

orientation > [int t]:
  ( TK_n  <<$t = Orientation::N; >>
  | TK_e  <<$t = Orientation::E; >>
  | TK_s  <<$t = Orientation::S; >>
  | TK_w  <<$t = Orientation::W; >>
  | TK_fn <<$t = Orientation::FN;>>
  | TK_fe <<$t = Orientation::FE;>>
  | TK_fs <<$t = Orientation::FS;>>
  | TK_fw <<$t = Orientation::FW;>>
  | TK_r0    <<$t = Orientation::N; >>
  | TK_r90   <<$t = Orientation::W; >>
  | TK_r180  <<$t = Orientation::S; >>
  | TK_r270  <<$t = Orientation::E; >>
  | TK_mx    <<$t = Orientation::FN;>>
  | TK_my    <<$t = Orientation::FE;>>
  | TK_mxr90 <<$t = Orientation::FS;>>
  | TK_myr90 <<$t = Orientation::FW;>>
  )
  ;

via_layer_rule < [Via *aVia]:
  <<Rect rect; std::vector<Rect>* rects = 0; ANTLRTokenPtr i; int layer_index;>>
  TK_layer tstring>[i] SEMI
    <<layer_index = find_layer_index(i);
      if (layer_index != -1) {
        rects = & $aVia->prepareLayer(layer_index);
      }
    >>
  ( via_rect>[rect] <<if (rects) rects->push_back(rect);>> )*
  ;

via_rect > [Rect rect]:
  << Pt sw,ne; >>
  TK_rect pt>[sw] pt>[ne] SEMI
  << $rect=Rect(sw,ne); >>
  ;






/* VIARULE STATEMENT BEGINS */

viarule:
  <<ViaRule* aViaRule = 0; ANTLRTokenPtr i1,i2;>>
  k:TK_viarule tstring>[i1]  <<aViaRule = viarules_.find_or_insert(text(i1));>>
    (        nongenerate_stmt<[aViaRule] <<number_viarules_++;>>
    | TK_generate            <<aViaRule->setGenerate();>>
                generate_stmt<[aViaRule] <<number_gviarules_++;>>
    )
    TK_end     tstring>[i2] <<checkEndConstruct(i1,i2,$k);>>
  ;

nongenerate_stmt < [ViaRule* aViaRule]:
  <<ViaRule_Layer *layer1,*layer2;>>
  viarule_layer_stmt>[layer1] viarule_layer_stmt>[layer2]
  <<if (aViaRule) aViaRule->setLayers(layer1,layer2);>>
  (viarule_via_stmt<[aViaRule])*
  (viarule_prop)*
  ;

generate_stmt < [ViaRule* aViaRule]:
  <<ViaRule_Layer *layer1,*layer2;>>
  viarule_layer_stmt>[layer1] viarule_layer_stmt>[layer2]
  <<if (layer1 && layer2) aViaRule->setLayers(layer1,layer2);>>
  {viarulegen_layer_stmt<[aViaRule]}
  ;

viarule_layer_stmt > [ViaRule_Layer* aLayer]:
  <<ANTLRTokenPtr i; int layer_index;>>
  TK_layer tstring>[i] SEMI
  <<layer_index = find_layer_index(i);
    if (layer_index != -1) {
      $aLayer = new ViaRule_Layer(layer_index);
    } else {
      $aLayer = 0;
    }
  >>
  (viarule_layer_option<[$aLayer])*
  ;

viarulegen_layer_stmt < [ViaRule* aViaRule]:
  <<ANTLRTokenPtr i; int layer_index; ViaRule_CutLayer* aLayer = 0;>>
  TK_layer tstring>[i] SEMI
  <<layer_index = find_layer_index(i);
    if (layer_index != -1) {
      aLayer = new ViaRule_CutLayer(layer_index);
    }
  >>
  (viarule_genlayer_option<[aLayer])*
  <<if (aLayer && aViaRule) aViaRule->setCutLayer(aLayer);>>
  ;

viarule_layer_option < [ViaRule_Layer* aLayer]:
  <<Unit u1,u2;>>
  ( TK_direction
      ( TK_horizontal  <<if (aLayer) aLayer->setDirection(Layer::HORIZONTAL); >>
      | TK_vertical    <<if (aLayer) aLayer->setDirection(Layer::VERTICAL);   >>
      )
  | TK_width unumber>[u1] TK_to unumber>[u2]
      <<if (aLayer) aLayer->setWidth(u1,u2);>>
  | TK_overhang unumber>[u1]
      <<if (aLayer) aLayer->setOverhang(u1);>>
  | TK_metaloverhang unumber>[u1]
      <<if (aLayer) aLayer->setMetalOverhang(u1);>>
  ) SEMI
  ;

viarule_genlayer_option < [ViaRule_CutLayer* aLayer]:
  <<Pt p1,p2; Unit u1,u2;>>
  ( TK_rect pt>[p1] pt>[p2]     <<if (aLayer) aLayer->setRect(Rect(p1,p2));>>
  | TK_spacing unumber>[u1] TK_by unumber>[u2]
                                <<if (aLayer) aLayer->setSpacing(u1,u2);>>
  | TK_resistance unumber>[u1]  <<if (aLayer) aLayer->setResistance(u1);>>
  ) SEMI
  ;

viarule_via_stmt < [ViaRule* aViaRule]:
  <<ANTLRTokenPtr i; int via_index;>>
  TK_via tstring>[i] SEMI
  <<via_index = find_via_index(i);
    if (via_index != -1) {
      aViaRule->addVia(via_index);
    }
  >>
  ;


viarule_prop: TK_property ( viarule_prop1 )+ SEMI
  ;

viarule_prop1:
  <<ANTLRTokenPtr i;>>
  tstring>[i] ( tstring>[i] | QSTRING | NUMBER )
  ;
        
/* VIARULE STATEMENT ENDS */



/***********************************************************************


viarule_keyword :
  <<ANTLRTokenPtr i;>>
  TK_viarule tstring>[i]
  ;

viarule_or_generate:
  viarule_keyword ( TK_generate viarule_generate_rest
                  | viarule_rest
                  )
  ;

viarule_rest:
  viarule_layer_list ( via_name )* ( viarule_prop )* end_viarule
    <<;>>
  ;

viarule_generate_rest:
  viarule_layer_list ( viarule_prop )* end_viarule
    <<;>>
  ;

viarule_layer_list : ( viarule_layer )+
  ;

viarule_prop: TK_property ( viarule_prop1 )+ SEMI
  ;

viarule_prop1:
  <<ANTLRTokenPtr i;>>
  tstring>[i] ( tstring>[i] | QSTRING | NUMBER )
  ;

viarule_layer: viarule_layer_name viarule_layer_options
  ;

via_name:
  <<ANTLRTokenPtr i;>>
  TK_via tstring>[i] SEMI
  ;

viarule_layer_name:
  <<ANTLRTokenPtr i;>>
  TK_layer tstring>[i] SEMI
  ;

viarule_layer_options: ( viarule_layer_option )*
  ;

viarule_layer_option:
  <<Pt p1,p2;>>
  ( TK_direction
      ( TK_horizontal
      | TK_vertical
      )SEMI
  | TK_width NUMBER TK_to NUMBER SEMI
    <<;>>
  | TK_overhang NUMBER SEMI
    <<;>>
  | TK_metaloverhang NUMBER SEMI
    <<;>>
  | TK_rect pt>[p1] pt>[p2] SEMI
    <<;>>
  | TK_spacing NUMBER TK_by NUMBER SEMI
    <<;>>
  | TK_resistance NUMBER SEMI
    <<;>>
  )
  ;


end_viarule:
  <<ANTLRTokenPtr i;>>
  TK_end tstring>[i] 
    <<;>>
  ;

*********************************************************/


spacing_rule: TK_spacing
               ( spacing )*
              TK_end TK_spacing 
  ;

spacing:
  <<Spacing* aSpacing = new Spacing; Unit n; ANTLRTokenPtr l1,l2;
    int lindex1, lindex2;>>
  TK_samenet
  tstring>[l1] tstring>[l2]
  <<lindex1 = find_layer_index(l1), lindex2 = find_layer_index(l2);
    aSpacing->setLayers(lindex1, lindex2);
  >>
  unumber>[n] <<aSpacing->setSpacing(n);>>
  { TK_stack  <<aSpacing->setStack();>> }
  SEMI <<if (lindex1 != -1 && lindex2 != -1) {
           spacings_.push_back(aSpacing);
         } else {
           delete aSpacing;
         }
       >>
  ;

irdrop: start_irdrop ir_tables end_irdrop
  ;

start_irdrop: TK_irdrop
  ;

end_irdrop: TK_end TK_irdrop
  ;

ir_tables: ( ir_table )*
  ;

ir_table: ir_tablename ir_table_values SEMI
  ;

ir_table_values: ( ir_table_value )*
  ;

ir_table_value: NUMBER NUMBER 
  ;

ir_tablename:
  <<ANTLRTokenPtr i;>>
  TK_table tstring>[i]
  ;

minfeature:
  <<Unit x,y;>>
  TK_minfeature unumber>[x] unumber>[y] SEMI
  <<minfeature_ = std::make_pair(x,y);>>
  ;

dielectric: TK_dielectric NUMBER SEMI
  ;

nondefault_rule:
  <<NondefRule* aRule; ANTLRTokenPtr i1,i2;>>
  k:TK_nondefaultrule tstring>[i1] <<aRule = nrules_.find_or_insert(text(i1));>>
                           (nd_rule<[aRule])*
    TK_end            tstring>[i2] <<checkEndConstruct(i1,i2,$k);>>
  ;

nd_rule < [NondefRule* aRule]:
  ( nd_layer<[aRule]
  | via
  | spacing_rule
  | nd_prop
  )
  ;

nd_prop: TK_property nd_prop_list SEMI
  ;

nd_prop_list: ( nd_prop1 )+
  ;

nd_prop1:
  <<ANTLRTokenPtr i;>>
  tstring>[i] ( tstring>[i] | QSTRING | NUMBER )
  ;

nd_layer < [NondefRule* aRule]:
  <<NDLayer *ndlayer; ANTLRTokenPtr i1,i2;>>
  k:TK_layer tstring>[i1] <<ndlayer = aRule->ndlayers_.find_or_insert(text(i1));>>
            (nd_layer_stmt<[ndlayer])*
    TK_end   tstring>[i2] <<checkEndConstruct(i1,i2,$k);>>
  ;
	
nd_layer_stmt < [NDLayer* ndlayer]:
  <<Unit n;>>
  ( TK_width unumber>[n]         <<ndlayer->setWidth(n);         >>
  | TK_spacing unumber>[n]       <<ndlayer->setSpacing(n);       >>
  | TK_wireextension unumber>[n] <<ndlayer->setWireextension(n); >>
  ) SEMI
  ;

site:
  <<Site *aSite; ANTLRTokenPtr i1,i2;>>
  k:TK_site tstring>[i1] <<aSite = sites_.find_or_insert(text(i1));>>
              (site_option[aSite])*
    TK_end  tstring>[i2] <<checkEndConstruct(i1,i2,$k);>>
                         <<is_standard_cell_ = true;>>
  ;

site_option[Site *aSite]:
  <<Unit x,y; int symm = Symmetry::UNKNOWN;>>
  ( TK_size unumber>[x] TK_by unumber>[y] SEMI
    <<$aSite->setSize(x,y);>>
  | TK_symmetry (symmetry[symm])* SEMI
    <<$aSite->setSymmetry(symm);>>
  | TK_class site_class<[aSite] SEMI
  )
  ;

site_class < [Site *aSite]:
  ( TK_pad     <<aSite->setClass(Site::PAD);    >>
  | TK_core    <<aSite->setClass(Site::CORE);   >>
  | TK_virtual <<aSite->setClass(Site::VIRTUAL);>>
  )
  ;

symmetry < [int& s]:
  ( TK_x   <<s|=Symmetry::X_SYMM;>>
  | TK_y   <<s|=Symmetry::Y_SYMM;>>
  | TK_r90 <<s|=Symmetry::R90_SYMM;>>
  )
  ;

pt > [Pt point]:
  (        unumber>[$point.x] unumber>[$point.y] 
  | LPAREN unumber>[$point.x] unumber>[$point.y] RPAREN
  )
  ;

macro:
  <<Macro *aMacro; ANTLRTokenPtr i1,i2;>>
  k:TK_macro tstring>[i1] <<aMacro = macros_.find_or_insert(text(i1));>>
              (macro_option[aMacro])*
    TK_end   tstring>[i2] <<checkEndConstruct(i1,i2,$k);>>
  ;

macro_option[Macro *aMacro]:
  <<int symm = Symmetry::UNKNOWN; Rat n; Unit x,y; Pt point;>>
  ( TK_class class_type<[$aMacro] SEMI
  | macro_generator 
  | macro_generate 
  | macro_source
  | TK_symmetry (symmetry[symm])* SEMI
      <<$aMacro->setSymmetry(symm);>>
  | TK_origin pt>[point] SEMI
      <<$aMacro->setOrigin(point);>>
  | TK_power number>[n] SEMI
      <<$aMacro->setPower(n);>>
  | macro_foreign[$aMacro]
  | macro_eeq 
  | macro_leq 
  | TK_size unumber>[x] TK_by unumber>[y] SEMI
      <<$aMacro->setSize(x,y);>>
  | macro_site[aMacro]
  | macro_pin[aMacro]
  | TK_function ( TK_buffer | TK_inverter ) SEMI
      <<;>>
  | macro_obs[aMacro]
  | macro_clocktype 
      <<;>>
  | timing
      <<;>>
  | TK_property <<;>> macro_prop_list  SEMI
      <<;>>
  )
  ;

macro_prop_list: ( macro_name_value_pair )+
  ;


macro_name_value_pair:
  <<ANTLRTokenPtr i,i1;>>
    tstring>[i] ( NUMBER | QSTRING | tstring>[i1] )
  ;

class_type <      [Macro* aMacro]:
  ( TK_cover    <<aMacro->setClass(MacroClass::COVER);   >>
  | TK_ring     <<aMacro->setClass(MacroClass::RING);    >>
  | TK_block    <<aMacro->setClass(MacroClass::BLOCK);   >>
  | TK_pad      <<aMacro->setClass(MacroClass::PAD);     >>
   { pad_type  <[$aMacro] }
  | TK_virtual  <<aMacro->setClass(MacroClass::VIRTUAL); >>
  | TK_core     <<aMacro->setClass(MacroClass::CORE);    >>
   { core_type <[$aMacro] }
  | TK_corner   <<aMacro->setClass(MacroClass::CORNER);  >>
  | TK_endcap   <<aMacro->setClass(MacroClass::ENDCAP);  >>
    endcap_type<[$aMacro]
  )
  ;

pad_type <        [Macro* aMacro]:
  ( TK_input    <<aMacro->setSubClass(MacroClass::INPUT);  >>
  | TK_output   <<aMacro->setSubClass(MacroClass::OUTPUT); >>
  | TK_inout    <<aMacro->setSubClass(MacroClass::INOUT);  >>
  | TK_power    <<aMacro->setSubClass(MacroClass::POWER);  >>
  | TK_spacer   <<aMacro->setSubClass(MacroClass::SPACER); >>
  )
  ;

core_type <       [Macro* aMacro]:
  ( TK_feedthru <<aMacro->setSubClass(MacroClass::FEEDTHRU); >>
  | TK_tiehigh  <<aMacro->setSubClass(MacroClass::TIEHIGH);  >>
  | TK_tielow   <<aMacro->setSubClass(MacroClass::TIELOW);   >>
  )
  ;

endcap_type <        [Macro* aMacro]:
  ( TK_pre         <<aMacro->setSubClass(MacroClass::PRE);    >>
  | TK_post        <<aMacro->setSubClass(MacroClass::POST);    >>
  | TK_topleft     <<aMacro->setSubClass(MacroClass::TOPLEFT);  >>
  | TK_topright    <<aMacro->setSubClass(MacroClass::TOPRIGHT);  >>
  | TK_bottomleft  <<aMacro->setSubClass(MacroClass::BOTTOMLEFT); >>
  | TK_bottomright <<aMacro->setSubClass(MacroClass::BOTTOMRIGHT); >>
  )
  ;

macro_generator:
  <<ANTLRTokenPtr i;>>
  TK_generator tstring>[i] SEMI
    <<;>>
  ;

macro_generate:
  <<ANTLRTokenPtr i1,i2;>>
  TK_generate tstring>[i1] tstring>[i2] SEMI
    <<;>>
  ;

macro_source:
    TK_source ( TK_user | TK_generate | TK_block ) SEMI
  ;

macro_foreign < [Macro *aMacro]:
  <<Foreign *f = 0;>>
  foreign>[f] <<aMacro->setForeign(f);>>
  ;

macro_eeq:
  <<ANTLRTokenPtr i;>>
  TK_eeq tstring>[i] SEMI
    <<;>>
  ;

macro_leq:
  <<ANTLRTokenPtr i;>>
  TK_leq tstring>[i] SEMI
    <<;>>
  ;

macro_site < [Macro* aMacro]:
  <<ANTLRTokenPtr i;>>
    macro_site_word
    ( tstring>[i]
      <<int site_index = sites_.find_index(text(i));
       SitePattern* sp = 0;>>
      { sitePattern_rest<[site_index]>[sp] }
      <<if (sp) aMacro->addSitePattern(sp);
       else     aMacro->addSite(site_index);>>
    ) SEMI
  ;

macro_site_word: TK_site
    <<;>>
  ;

site_word: TK_site
    <<;>>
  ;

/* This is confusing, since FEF and LEF have opposite definitions of
   ports and pins */

macro_pin < [Macro* aMacro]:
  <<MacroPin* aPin; ANTLRTokenPtr i1,i2;>>
  k:TK_pin tstring>[i1] <<aPin = $aMacro->addMacroPin(text(i1));>>
               ( macro_pin_option[aMacro,aPin] )*
    TK_end tstring>[i2] <<checkEndConstruct(i1,i2,$k);>>
  ;

macro_pin_option < [Macro* aMacro, MacroPin* aPin]:
  << Rat n,n1; Pt p; int ori; ANTLRTokenPtr foreign_id,i,i1;>>
  ( start_foreign>[foreign_id]
    { pt>[p] {orientation>[ori]}
    | TK_structure 
      {pt>[p] {orientation>[ori]} }
    } SEMI
  | TK_leq tstring>[i] SEMI
    <<;>>
  | TK_power number>[n] SEMI
    <<$aPin->setPower(n);>>
  | TK_direction electrical_direction<[$aPin] SEMI
  | TK_use macro_pin_use<[$aPin] SEMI
  | TK_scanuse macro_scan_use<[$aPin] SEMI
  | TK_leakage number>[n] SEMI
    <<$aPin->setLeakage(n);>>
  | TK_risethresh number>[n] SEMI
    <<;>>
  | TK_fallthresh number>[n] SEMI
    <<;>>
  | TK_risesatcur number>[n] SEMI
    <<;>>
  | TK_fallsatcur number>[n] SEMI
    <<;>>
  | TK_vlo number>[n] SEMI
    <<$aPin->setVlo(n);>>
  | TK_vhi number>[n] SEMI
    <<$aPin->setVhi(n);>>
  | TK_tieoffr number>[n] SEMI
    <<;>>
  | TK_shape {pin_shape<[$aPin]} SEMI
  | TK_mustjoin tstring>[i] SEMI
    <<$aPin->addMustjoin(aMacro->getMacroPinIndex(text(i)));>>
  | TK_outputnoisemargin <<;>> number>[n] number>[n1] SEMI
    <<;>>
  | TK_outputresistance <<;>>  number>[n] number>[n1] SEMI
    <<;>>
  | TK_inputnoisemargin <<;>>  number>[n] number>[n1] SEMI
    <<;>>
  | TK_capacitance number>[n] SEMI 
    <<$aPin->setCapacitance(n);>>
  | TK_maxdelay number>[n] SEMI 
    <<;>>
  | TK_maxload number>[n] SEMI 
    <<;>>
  | TK_resistance number>[n] SEMI 
    <<$aPin->setResistance(n);>>
  | TK_pulldownres number>[n] SEMI 
    <<;>>
  | TK_currentsource ( TK_active | TK_resistive ) SEMI 
    <<;>>
  | TK_risevoltagethreshold number>[n] SEMI 
    <<;>>
  | TK_fallvoltagethreshold number>[n] SEMI 
    <<;>>
  | TK_iv_tables tstring>[i] tstring>[i1] SEMI
    <<;>>
  | TK_taperrule tstring>[i] SEMI
    <<;>>
  | TK_property <<;>> pin_prop_list SEMI
    <<;>>
  | start_macro_port
    (geometry<[$aPin])*
    TK_end
    <<;>>
  | TK_antennasize number>[n] {TK_layer tstring>[i]} SEMI
    <<;>>
  | TK_antennametalarea number>[n] {TK_layer tstring>[i]} SEMI
    <<;>>
  | TK_antennametallength NUMBER {TK_layer tstring>[i]} SEMI
    <<;>>
  | TK_riseslewlimit NUMBER SEMI
    <<;>>
  | TK_fallslewlimit NUMBER SEMI
    <<;>>
  )
  ;
    

pin_prop_list: ( pin_name_value_pair )+
  ;

pin_name_value_pair:
  <<ANTLRTokenPtr i,i1; Rat n;>>
  tstring>[i] ( number>[n] | QSTRING | tstring>[i1] )
  ;

electrical_direction <               [MacroPin* aPin]:
  ( TK_input     <<aPin->setDirection(MacroPin::Direction::INPUT);   >>
  | TK_output    <<aPin->setDirection(MacroPin::Direction::OUTPUT);  >>
  { TK_tristate  <<aPin->setDirection(MacroPin::Direction::TRISTATE);>> }
  | TK_inout     <<aPin->setDirection(MacroPin::Direction::INOUT);   >>
  | TK_feedthru  <<aPin->setDirection(MacroPin::Direction::FEEDTHRU);>>
  )
  ;

start_macro_port: TK_port {TK_class (TK_none | TK_core) SEMI}
    <<;>>
  ;

macro_pin_use <                 [MacroPin* aPin]:
  ( TK_signal     <<aPin->setUse(MacroPin::Use::SIGNAL); >>
  | TK_analog     <<aPin->setUse(MacroPin::Use::ANALOG); >>
  | TK_power      <<aPin->setUse(MacroPin::Use::POWER);  >>
  | TK_ground     <<aPin->setUse(MacroPin::Use::GROUND); >>
  | TK_clock      <<aPin->setUse(MacroPin::Use::CLOCK);  >>
  | TK_data       <<aPin->setUse(MacroPin::Use::DATA);   >>
  )
  ;

macro_scan_use <                   [MacroPin* aPin]:
  ( TK_input     <<aPin->setScanUse(MacroPin::ScanUse::INPUT);  >>
  | TK_output    <<aPin->setScanUse(MacroPin::ScanUse::OUTPUT); >>
  | TK_start     <<aPin->setScanUse(MacroPin::ScanUse::START);  >>
  | TK_stop      <<aPin->setScanUse(MacroPin::ScanUse::STOP);   >>
  )
  ;

pin_shape <                      [MacroPin* aPin]:
  ( TK_abutment  <<aPin->setShape(MacroPin::Shape::ABUTMENT); >>
  | TK_ring      <<aPin->setShape(MacroPin::Shape::RING);     >>
  | TK_feedthru  <<aPin->setShape(MacroPin::Shape::FEEDTHRU); >>
  )
  ;

geometry < [Geometries* geom]:
    layer_geometry<[$geom]
  | via_geometry<[$geom]
  ;

layer_geometry < [Geometries* g]:
  <<ANTLRTokenPtr i; int layer_index; Unit width = Unit(-1);
    LayerGeometry::Path* path = 0; Pt p,sw,ne; StepPattern sp;
    LayerGeometry* geom = 0;
  >>
  TK_layer tstring>[i] SEMI
    <<layer_index = find_layer_index(i); geom = 0;>>
    (  <<if (layer_index != -1) geom = new LayerGeometry(layer_index);>>
       { TK_width unumber>[width] SEMI }
       ( TK_path <<if (width == Unit(-1)) {
                       width = getLayer(layer_index).getWidth();
                   }
                   if (geom) path = geom->addPath(width);>>
         ( pt>[p] <<if (path) path->addPathPoint(p);>>
           ( pt>[p] <<if (path) path->addPathPoint(p);>>)*
         | TK_iterate <<if (geom) geom->setIterate();>>
           pt>[p] <<if (path) path->addPathPoint(p);>>
           ( pt>[p] <<if (path) path->addPathPoint(p);>>)*
           stepPattern>[sp] <<if (geom) geom->addStepPattern(sp);>>
         ) SEMI <<width = Unit(-1);>>
       | TK_rect
         ( pt>[sw] pt>[ne]
         | TK_iterate <<if (geom) geom->setIterate();>>
           pt>[sw] pt>[ne]
           stepPattern>[sp] <<if (geom) geom->addStepPattern(sp);>>
         ) SEMI <<if (geom) geom->addRect(Rect(sw,ne));>>
       | TK_polygon
         ( firstPt>[p] <<if (geom) geom->addPolyPoint(p);>>
           nextPt>[p]  <<if (geom) geom->addPolyPoint(p);>>
           nextPt>[p]  <<if (geom) geom->addPolyPoint(p);>>
           nextPt>[p]  <<if (geom) geom->addPolyPoint(p);>>
          (nextPt>[p]  <<if (geom) geom->addPolyPoint(p);>>)*
         | TK_iterate firstPt>[p] nextPt>[p] nextPt>[p] nextPt>[p]
          (nextPt>[p])* stepPattern>[sp]
         ) SEMI
       )
     )*
    <<if (geom) $g->push_back(geom);>>
  ;

via_geometry < [Geometries* g]:
    <<ViaGeometry* geom = new ViaGeometry();>>
  via_placement<[geom]
    <<$g->push_back(geom);>>
  ;

firstPt > [Pt p]:
  pt>[$p]
  ;

nextPt > [Pt p]:
  pt>[$p]
  ;

via_placement < [ViaGeometry* geom]:
  <<Pt p; StepPattern sp; ANTLRTokenPtr i;>>
  TK_via
  ( pt>[p] tstring>[i] <<$geom->setVia(find_via_index(i));>>
  | TK_iterate <<$geom->setIterate();>>
    pt>[p] tstring>[i] <<$geom->setVia(find_via_index(i));>>
    stepPattern>[sp] <<$geom->addStepPattern(sp);>>
  ) SEMI <<$geom->addPoint(p);>>
  ;

stepPattern > [StepPattern pat]:
  <<int num_x,num_y; Unit step_x,step_y;>>
  TK_do    inumber>[num_x] TK_by inumber>[num_y]
  TK_step  unumber>[step_x]      unumber>[step_y]
  <<$pat = StepPattern(num_x,num_y,step_x,step_y);>>
  ;

sitePattern > [SitePattern *sitepat]:
  <<int site_index; ANTLRTokenPtr i;>>
  tstring>[i]
  <<site_index = sites_.find_index(text(i));>>
  sitePattern_rest<[site_index]>[$sitepat]
  ;

sitePattern_rest < [int site_index]
                 > [SitePattern *sitepat]:
  <<Unit ori_x, ori_y; int orient;>>
  unumber>[ori_x] unumber>[ori_y]
  orientation>[orient]
  <<StepPattern steppat;>>
  stepPattern>[steppat]
  <<$sitepat = new SitePattern(site_index,ori_x,ori_y,orient,steppat);>>
  ;

trackPattern > [TrackPattern *trackpat]:
  <<bool x_dir; Unit start, space; int numTracks; >>
  ( TK_x <<x_dir = true;>> | TK_y <<x_dir=false;>> )
  unumber>[start] TK_do inumber>[numTracks] TK_step unumber>[space]
  <<$trackpat = new TrackPattern(x_dir,start,numTracks,space);>>
  { TK_layer trackLayers<[$trackpat] }
  ;

trackLayers < [TrackPattern *trackpat]:
  <<ANTLRTokenPtr i;>>
  ( tstring>[i]
    <<int layer_index = layers_.find_index(text(i));
     if (layer_index == -1) {
       db::warn2(i,"unresolved layer name");
     }
     $trackpat->addLayer(layer_index);
    >>
  )*
  ;

gcellPattern > [GcellPattern *gcelpat]:
  <<bool x_dir; Unit start, space; int num; >>
  ( TK_x <<x_dir = true;>> | TK_y <<x_dir=false;>> )
  unumber>[start] TK_do inumber>[num] TK_step unumber>[space]
  <<$gcelpat = new GcellPattern(x_dir,start,num,space);>>
  ;


macro_obs < [Macro* aMacro]:
  <<Geometries geom;>>
  TK_obs
  (geometry<[&geom])*
  TK_end
  <<aMacro->addMacroObs(geom);>>
  ;

macro_clocktype:
  <<ANTLRTokenPtr i;>>
  TK_clocktype tstring>[i] SEMI
    <<;>>
  ;

timing: start_timing (timing_option)* end_timing
    <<;>>
  ;

start_timing: TK_timing
    <<;>>
  ;

end_timing: TK_end TK_timing
  <<;>>
  ;

timing_option:
  <<ANTLRTokenPtr i; Rat n1,n2,n3;>>
  ( TK_frompin (tstring>[i])+ SEMI
  | TK_topin   (tstring>[i])+ SEMI
  | risefall 
    ( TK_intrinsic number>[n1] number>[n2]
    <<;>>
    slew_spec TK_variable number>[n1] number>[n2]
    <<;>>
    | delay_or_transition TK_unateness unateness
    TK_tabledimension NUMBER NUMBER NUMBER
    <<;>>
    ) SEMI
  | TK_tableaxis list_of_table_axis_numbers SEMI
    <<;>>
  | TK_tableentries list_of_table_entries SEMI
    <<;>>
  | TK_risers NUMBER NUMBER SEMI
    <<;>>
  | TK_fallrs NUMBER NUMBER SEMI
    <<;>>
  | TK_risecs NUMBER NUMBER SEMI
    <<;>>
  | TK_fallcs NUMBER NUMBER SEMI
    <<;>>
  | TK_risesatt1 NUMBER NUMBER SEMI
    <<;>>
  | TK_fallsatt1 NUMBER NUMBER SEMI
    <<;>>
  | TK_riset0 NUMBER NUMBER SEMI
    <<;>>
  | TK_fallt0 NUMBER NUMBER SEMI
    <<;>>
  | TK_unateness unateness SEMI
    <<;>>
  | TK_stable TK_setup NUMBER TK_hold NUMBER risefall SEMI
    <<;>>
  | two_pin_trigger from_pin_trigger to_pin_trigger
    TK_tabledimension NUMBER NUMBER NUMBER SEMI
    <<;>>
  | one_pin_trigger TK_tabledimension NUMBER NUMBER NUMBER SEMI 
    <<;>>
  | TK_sdfcondstart QSTRING SEMI
    <<;>>
  | TK_sdfcondend QSTRING SEMI
    <<;>>
  | TK_sdfcond QSTRING SEMI
    <<;>>
  | TK_extension SEMI
    <<;>>
  )
  ;

one_pin_trigger:
    TK_mpwh
    <<;>>
  | TK_mpwl
    <<;>>
  | TK_period
    <<;>>
  ;

two_pin_trigger :
    TK_setup
    <<;>>
  | TK_hold
    <<;>>
  | TK_recovery
    <<;>>
  | TK_skew
    <<;>>
  ;

from_pin_trigger:
    TK_anyedge
    <<;>>
  | TK_posedge
    <<;>>
  | TK_negedge
    <<;>>
  ;

to_pin_trigger:
    TK_anyedge
    <<;>>
  | TK_posedge
    <<;>>
  | TK_negedge
    <<;>>
  ;

delay_or_transition :
    TK_delay
    <<;>>
  | TK_transitiontime
    <<;>>
  ;

list_of_table_entries: ( table_entry )+
  ;

table_entry: LPAREN NUMBER NUMBER NUMBER RPAREN
    <<>>
  ;

list_of_table_axis_numbers: ( NUMBER )+
  ;

slew_spec:
  /* empty */
    <<;>>
  | NUMBER NUMBER NUMBER NUMBER {NUMBER NUMBER NUMBER}
    <<;>>
  ;

risefall:
    TK_rise
    <<;>>
  | TK_fall 
    <<;>>
  ;

unateness:
    TK_invert
    <<;>>
  | TK_noninvert
    <<;>>
  | TK_nonunate
    <<;>>
  ;

array:
  <<Array *lefArray; ANTLRTokenPtr i1,i2;>>
  k:TK_array tstring>[i1] <<lefArray = arrays_.find_or_insert(text(i1));>>
                (array_rule[lefArray])*
    TK_end   tstring>[i2] <<checkEndConstruct(i1,i2,$k);>>
                          <<is_gate_array_ = true;>>
  ;

array_rule < [Array *array]:
  << SitePattern* sp; ANTLRTokenPtr i1,i2;>>
  ( TK_site          sitePattern>[sp] SEMI
   <<$array->addSitePattern(sp);>>
  | TK_canplace      sitePattern>[sp] SEMI
    <<$array->addCanplace(sp);>>
  | TK_cannotoccupy  sitePattern>[sp] SEMI
    <<$array->addCannotoccupy(sp);>>
  | TK_tracks  <<TrackPattern* tp;>>   trackPattern>[tp] SEMI
    <<$array->addTrackPattern(tp);>>
  | k:TK_floorplan tstring>[i1]
       <<Floorplan* aFp = $array->getFloorplans().find_or_insert(text(i1));>>
        (floorplan_element<[aFp])*
      TK_end       tstring>[i2] <<checkEndConstruct(i1,i2,$k);>>
    <<;>>
  | TK_gcellgrid <<GcellPattern* gp;>> gcellPattern>[gp] SEMI
    <<$array -> addGcellPattern(gp);>>
  | TK_defaultcap NUMBER cap_list TK_end TK_defaultcap
    <<;>>
  | def_statement
    <<;>>
  )
  ;

floorplan_element < [Floorplan* aFp]:
  <<SitePattern* sp;>>
  ( TK_canplace      sitePattern>[sp] SEMI
    <<aFp->addCanplace(sp);>>
  | TK_cannotoccupy  sitePattern>[sp] SEMI
    <<aFp->addCannotoccupy(sp);>>
  )
  ;

cap_list: ( one_cap )*
  ;

one_cap: TK_minpins NUMBER TK_wirecap NUMBER SEMI
    <<;>>
  ;

msg_statement:
  <<ANTLRTokenPtr i;>>
  TK_message tstring>[i] ASSIGN s_expr dtrm
    <<;>>
  ;

create_file_statement:
  <<ANTLRTokenPtr i;>>
  TK_createfile tstring>[i] ASSIGN s_expr dtrm
    <<;>>
  ;

def_statement:
  <<ANTLRTokenPtr i,i1;>>
  ( TK_define   tstring>[i] ASSIGN expression dtrm
    <<;>>
  | TK_defines  tstring>[i] tstring>[i1] ASSIGN s_expr dtrm
    <<;>>
  | TK_defineb  tstring>[i] tstring>[i1] ASSIGN b_expr dtrm
    <<;>>
  )
  ;

/* terminator for &defines.  Can be semicolon or newline */
dtrm:
  |  SEMI <<>>
  //|  NL	<<>>
  ;

then_rule:
  TK_then
  //| NL TK_then
  ;

else_rule:
  TK_else
  //| NL TK_else
  ;

/****
expression:
    expression PLUS expression    <<;>>
  | expression MINUS expression   <<;>>
  | expression STAR expression    <<;>>
  | expression DIV expression     <<;>>
  | MINUS expression // %prec UMINUS <<;>>
  | LPAREN expression RPAREN		<<;>>
  | TK_if b_expr then_rule expression else_rule expression // %prec IF
		<<;>>
  | NUMBER
  ;

b_expr:
  expression relop expression <<;>>
  | expression TK_and expression <<;>>
  | expression TK_or  expression <<>>
  | s_expr relop s_expr	      <<>>
  | s_expr TK_and s_expr	      <<>>
  | s_expr TK_or  s_expr	      <<>>
  | b_expr TK_eq b_expr	      <<>>
  | b_expr TK_ne b_expr	      <<>>
  | b_expr TK_and b_expr	      <<>>
  | b_expr TK_or  b_expr	      <<>>
  | TK_not b_expr		     // %prec LNOT <<>>
  | LPAREN b_expr RPAREN	      <<;>>
  | TK_if b_expr then_rule b_expr else_rule b_expr  // %prec IF
    <<;>>
  | TK_true		      <<;>>
  | TK_false		      <<;>>
  ;

s_expr:
  s_expr PLUS s_expr
    <<>>
  | LPAREN s_expr RPAREN
    <<>>
  | TK_if b_expr then_rule s_expr else_rule s_expr // %prec IF
    <<>> else <<>>
  | QSTRING
    <<>>
  ;

relop:
    TK_le <<>>
  | TK_lt <<>>
  | TK_ge <<>>
  | TK_gt <<>>
  | TK_eq <<>>
  | TK_ne <<>>
  | ASSIGN  <<>>
  | LT_  <<>>
  | GT  <<>>
  ;
****/

expression:
  add_expr
  ;

add_expr:
  mult_expr ( ( PLUS | MINUS ) mult_expr )*
  ;

mult_expr:
  unary_expr ( ( STAR | DIV ) unary_expr )* 
  ;

unary_expr:
     ( PLUS | MINUS ) primary_expr
  |  primary_expr
  ;

primary_expr:
    LPAREN expression RPAREN
  | TK_if b_expr then_rule expression else_rule expression // %prec IF
    <<;>>
  |  NUMBER
  ;


b_expr:
  or_expr
  ;

or_expr:
  and_expr ( TK_or and_expr )*
  ;

and_expr:
  eq_expr ( TK_and eq_expr )*
  ;

eq_expr:
  unary_bexpr ( ( TK_eq | TK_ne ) unary_bexpr )*
  ;

unary_bexpr:
     TK_not primary_bexpr
  |  primary_bexpr
  ;

primary_bexpr:
    expression ( relop | TK_and | TK_or ) expression
  | s_expr ( relop | TK_and | TK_or ) s_expr
  | LPAREN b_expr RPAREN	      <<;>>
  | TK_if  b_expr then_rule b_expr else_rule b_expr  // %prec IF
    <<;>>
  | TK_true		      <<;>>
  | TK_false		      <<;>>
 ;


s_expr:
  s_primary ( PLUS s_primary )*
    <<>>
  ;

s_primary:
    LPAREN s_expr RPAREN
    <<;>>
  | TK_if b_expr then_rule s_expr else_rule s_expr // %prec IF
    <<;>> else_rule <<;>>
  | QSTRING
    <<;>>
  ;

relop:
    TK_le <<;>>
  | TK_lt <<;>>
  | TK_ge <<;>>
  | TK_gt <<;>>
  | TK_eq <<;>>
  | TK_ne <<;>>
  | ASSIGN  <<;>>
  | LT_     <<;>>
  | GT      <<;>>
  ;

prop_def_section: TK_propertydefinitions
  ( prop_stmt )*
  TK_end TK_propertydefinitions
  ;

prop_stmt:
  <<ANTLRTokenPtr i;>>
  ( TK_library
    tstring>[i] prop_define SEMI
    <<;>>
  | TK_componentpin
    tstring>[i] prop_define SEMI
    <<;>>
  | TK_pin
    tstring>[i] prop_define SEMI
    <<;>>
  | TK_macro
    tstring>[i] prop_define SEMI
    <<;>>
  | TK_via
    tstring>[i] prop_define SEMI
    <<;>>
  | TK_viarule
    tstring>[i] prop_define SEMI
    <<;>>
  | TK_layer
    tstring>[i] prop_define SEMI
    <<;>>
  | TK_nondefaultrule
    tstring>[i] prop_define SEMI
    <<;>>
  )
  ;
    
prop_define:
  <<ANTLRTokenPtr i;>>
  ( TK_integer opt_def_range opt_def_value 
    <<;>>
  | TK_real opt_def_range opt_def_value
    <<;>>
  | TK_string {QSTRING <<;>>}
    <<;>>
  | TK_namemapstring tstring>[i]
    <<;>>
  )
  ;

range > [LEFParse::UnitRange r]:
  <<Unit min,max;>>
  ( TK_range unumber>[min] unumber>[max] { TK_uselengththreshold }
    <<$r.first = min; $r.second = max;>>
  | TK_lengththreshold unumber>[min]
    <<$r.first = min; $r.second = min;>>
  )
  ;

opt_def_range:
  /* nothing */
    <<;>>
  | TK_range NUMBER NUMBER
    <<;>>
  ;

opt_def_value:
  /* empty */
    <<;>>
  | NUMBER
    <<;>>
  ;

universalnoisemargin: TK_universalnoisemargin NUMBER NUMBER SEMI
    <<;>>
  ;

edgeratethreshold1: TK_edgeratethreshold1 NUMBER SEMI
    <<;>>
  ;

edgeratethreshold2: TK_edgeratethreshold2 NUMBER SEMI
    <<;>>
  ;

edgeratescalefactor: TK_edgeratescalefactor NUMBER SEMI
    <<;>>
  ;

noisetable: TK_noisetable NUMBER
    <<;>>
    SEMI noise_table_list TK_end TK_noisetable dtrm
    <<;>>
  ;

noise_table_list: ( noise_table_entry )+
  ;

noise_table_entry:
    TK_edgerate NUMBER SEMI
    <<;>>
  | output_resistance_entry
    <<;>>
  ;

output_resistance_entry: TK_outputresistance
    <<;>>
    num_list SEMI ( victim )+
  ;

num_list: ( NUMBER )+
  ;

victim: TK_victimlength NUMBER SEMI
  <<;>>
      TK_victimnoise vnoiselist SEMI
  <<;>>
  ;

vnoiselist: ( NUMBER )+
  ;

correctiontable: TK_correctiontable NUMBER SEMI
    <<;>>
    correction_table_list TK_end TK_correctiontable dtrm
    <<;>>
  ;

correction_table_list: ( correction_table_item )+
  ;

correction_table_item:
  TK_edgerate NUMBER SEMI
    <<;>>
  | output_list
    <<;>>
  ;

output_list: TK_outputresistance
  <<;>>
  numo_list SEMI ( corr_victim )+
  <<;>>
  ;

numo_list: ( NUMBER )+
  ;

corr_victim:
  TK_victimlength NUMBER SEMI
     <<;>>
  TK_correctionfactor corr_list SEMI
     <<;>>
  ;

corr_list: ( NUMBER )+
  ;

input_antenna: TK_inputpinantennasize NUMBER SEMI
    <<;>>
  ;

output_antenna: TK_outputpinantennasize NUMBER SEMI
    <<;>>
  ;

inout_antenna: TK_inoutpinantennasize NUMBER SEMI
    <<;>>
  ;

extension: TK_beginext (~TK_endext)* TK_endext
    <<;>>
  ;


number > [Rat num] :
  n:NUMBER
    <<$num = Rat(n->getText());>>
  ;


inumber > [int n] :
  <<Rat r;>>
  t:NUMBER
  <<r = Rat(t->getText());
   if (r.den() == 1) {
     $n = int(r.num());
   } else {
     //db::warn(t," expected integer : float rounded to the nearest");
     $n = round2nearest(r);
   }
   >>
  ;

unumber > [Unit n] :
  <<Rat r;>>
  t:NUMBER
  <<r = Rat(t->getText());
    r *= units_.getLefMicrons();
    if (r.den() == 1) {
      $n = Unit(r.num());
    } else {
      //db::warn(t, " expected integer after multiplying by %ld :"
      //        " float rounded to the nearest",
      //        units_.getLefMicrons());
      $n = round2nearest(r); 
    }
  >>
  ;

tstring > [ANTLRTokenPtr tok] :
  ( t1:T_STRING             <<$tok = $t1;>>
  | t2:TK_abutment .. TK_y  <<$tok = $t2;>>
  | t3:NUMBER               <<$tok = $t3;>>
  )
  ;


}
