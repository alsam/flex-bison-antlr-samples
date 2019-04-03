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

//  DEF grammar parser using PCCTS.
//
// $Id: DEFparse.g,v 1.1.4.26 2003/04/03 10:12:05 spike Exp $
//

#header <<
  #include "design_interface/strdup.h"
  #include "design_interface/db_support.h"
  #include "design_interface/parser.h"

  using db::DBSupport;

  #define PURIFY(r,s)
  #define PCCTS_PURIFY(r,s)
>>

<<
  using namespace db;

  typedef ANTLRCommonToken ANTLRToken;

>>


class DEFParse : public DBSupport
{

<<
protected:

  typedef std::pair<Component*, bool> ComponentTuple;
  typedef std::pair<Net*,       bool> NetTuple;
  typedef std::pair<Pin*,       bool> PinTuple;
  typedef Net::route_container        RouteContainer;
  typedef std::list<Pt>               RoutePath;

  static int numModels, numInstances, numNets;
  static int numPlaced, numPreplaced, numFixed;

private:

    ANTLRTokenPtr sink;
    Pt pp;

    bool regexp_seen;
>>

#token TK_version		  
#token TK_namescasesensitive	  
#token TK_namemapstring	  
#token TK_history		  
#token TK_design		  
#token TK_vias  		 
#token TK_tech  		 
#token TK_units 		 
#token TK_busbitchars		 
#token TK_dividerchar		 
#token TK_distance		 
#token TK_microns		 
#token TK_rect  		 
#token TK_reentrantpaths	 
#token TK_site  		 
#token TK_canplace		 
#token TK_cannotoccupy  	 
#token TK_diearea		 
#token TK_pins  		 
#token TK_defaultcap		 
#token TK_minpins		 
#token TK_wirecap		 
#token TK_tracks		 
#token TK_gcellgrid		 
#token TK_do			 
#token TK_by			 
#token TK_step  		 
#token TK_layer 		 
#token TK_components		 
#token TK_generate		 
#token TK_source		 
#token TK_weight		 
#token TK_fixed 		 
#token TK_cover 		 
#token TK_placed
#token TK_placement
#token TK_unplaced		 
#token TK_foreign		 
#token TK_eeqmaster		 
#token TK_region		 
#token TK_regions		 
#token TK_nets  		 
#token TK_mustjoin		 
#token TK_original		 
#token TK_use			 
#token TK_style 		 
#token TK_pattern		 
#token TK_patternname		 
#token TK_nondefaultrule	 
#token TK_estcap		 
#token TK_routed		 
#token TK_row			 
#token TK_taper 		 
#token TK_taperrule		 

#token TK_new			 
#token TK_shape 		 
#token TK_snets 		 
#token TK_snet  		 
#token TK_width 		 
#token TK_voltage		 
#token TK_spacing		  

#token TK_on
#token TK_off

#token TK_n			  
#token TK_s			 
#token TK_e			 
#token TK_w			 
#token TK_fn			 
#token TK_fs			 
#token TK_fe			 
#token TK_fw			 

#token TK_groups		 
#token TK_group 		 
#token TK_component		 
#token TK_soft  		 
#token TK_maxx  		 
#token TK_maxy  		 
#token TK_maxhalfperimeter	 
#token TK_constraints		 
#token TK_net			 
#token TK_path  		 
#token TK_sum			 
#token TK_diff  		 
#token TK_risemin		 
#token TK_risemax		 
#token TK_fallmin		 
#token TK_fallmax		 
#token TK_wiredlogic		 
#token TK_maxdist		 
#token TK_assertions		 
#token TK_end			 
#token TK_array 		 
#token TK_floorplan		  

#token TK_scanchains		 
#token TK_start 		 
#token TK_floating		 
#token TK_ordered		 
#token TK_stop  		 
#token TK_in			 
#token TK_out			 

#token TK_timingdisables	 
#token TK_iotimings		 
#token TK_rise  		 
#token TK_fall  		 
#token TK_variable		 
#token TK_slewrate		 
#token TK_capacitance		 
#token TK_drivecell		 
#token TK_frompin		 
#token TK_topin 		 
#token TK_parallel		 
#token TK_partitions		 
#token TK_turnoff		 
#token TK_fromclockpin  	 
#token TK_fromcomppin		 
#token TK_fromiopin		 
#token TK_toclockpin		 
#token TK_tocomppin		 
#token TK_toiopin		 
#token TK_setuprise		 
#token TK_setupfall		 
#token TK_holdrise		  
#token TK_holdfall		  

#token TK_vpin 		  
#token TK_subnet
#token TK_xtalk

#token TK_pin  		  

#token TK_synthesized  	  

#token TK_special		  
#token TK_direction		  
#token TK_range		  

#token TK_fpc			 
#token TK_horizontal		 
#token TK_vertical		 
#token TK_align 		 
#token TK_min			 
#token TK_max			 
#token TK_equal 		 
#token TK_bottomleft		  
#token TK_topright		  
#token TK_rows 		  


#token TK_input     
#token TK_inout     
#token TK_output    
#token TK_feedthru  

#token TK_signal    
#token TK_power     
#token TK_ground    
#token TK_clock     
#token TK_tieoff    
#token TK_analog    

#token TK_ring
#token TK_stripe
#token TK_followpin
#token TK_iowire
#token TK_blockwire
#token TK_corewire
#token TK_fillwire
#token TK_blockagewire

#token TK_x
#token TK_y

#token TK_propertydefinitions	 
#token TK_property		 
#token TK_string		 
#token TK_real  		 
#token TK_integer		 
#token TK_beginext		 
#token TK_endext		 
#token TK_pinproperties 	 
#token TK_commonscanpins	 
#token TK_componentpin  	 
#token TK_shield		 
#token TK_shieldnet		 
#token TK_noshield

#token TK_blockages
#token TK_slots
#token TK_fills
#token TK_pushdown

#token NUMBER
#token SEMI
#token T_STRING
#token QSTRING

#token LPAREN
#token RPAREN
#token STAR
#token MINUS

//#token COMMA
//#token EQUAL

#token NONE
#token Eof "@"

#errclass IDENTIFIER {tstring}

start_translation:
  << def_namescasesensitive_ = lef_namescasesensitive_; >>
  def_file
  Eof
  ;


def_file: {version_stmt} {case_sensitivity} (rule)* {end_design}
        ;

version_stmt:
  TK_version number>[def_version_] SEMI
  ;

case_sensitivity: TK_namescasesensitive
  ( TK_on
    <<def_namescasesensitive_ = true;
      string_compare::set_case_sensitive();   >>
  | TK_off
    <<def_namescasesensitive_ = false;
      string_compare::set_case_insensitive(); >>
  ) SEMI
  ;

rule:   design_section | via_section | extension_section| comps_section
      | nets_section | snets_section | groups_section | scanchains_section
      | constraint_section | assertions_section | iotiming_section
      | regions_section | floorplan_contraints_section
      | timingdisables_section | partitions_section | pin_props_section
      | blockages_section // 5.4 - 5.5 extension
      ;

design_section: design_name | tech_name | history | units 
              | divider_char | bus_bit_chars
              | site | canplace | cannotoccupy | die_area
              | pin_cap_rule | pin_rule | tracks_rule | gcellgrid | array_name
              | floorplan_name | row_rule | prop_def_section
              ;

design_name:
  <<ANTLRTokenPtr t;>>
  TK_design tstring>[t] SEMI
  <<if (t != 0) {
      design_name_ = std::string(text(t));
    } else {
      design_name_ = std::string("");
      err("missed DESIGN name");
    }
  >>
  ;

end_design: TK_end TK_design
  <<;>>
  ;

tech_name:
  <<ANTLRTokenPtr t;>>
  TK_tech tstring>[t] SEMI
  <<if (t != 0) {
      tech_name_ = std::string(text(t));
    } else {
      tech_name_ = std::string("");
      err("missed TECH name");
    }
  >>
  ;

array_name:
  <<ANTLRTokenPtr t;>>
  TK_array tstring>[t] SEMI
  <<if (t != 0) {
      array_index_ = arrays_.find_index(text(t));
    } else {
      array_index_ = -1;
    }
    if (array_index_ == -1) {
      if (t != 0) db::err2(t,"undefined ARRAY name");
      else err("missed ARRAY name");
    }
  >>
  ;

floorplan_name:
  <<ANTLRTokenPtr t; const char* fpname;>>
  TK_floorplan tstring>[t] SEMI
  <<if (t != 0) {
      fpname = text(t);
      if (strcasecmp(fpname, "default") == 0) {
        floorplan_index_ = -2;
      } else {
        if (0 <= array_index_ && array_index_ < arrays_.size()) {
          mvector<Floorplan*>& fp = arrays_[array_index_]->getFloorplans();
          floorplan_index_ = fp.find_index(fpname);
        }
      }
    } else {
      floorplan_index_ = -1;
    }
    if (floorplan_index_ == -1) {
      if (t != 0) db::err2(t,"undefined FLOORPLAN name");
      else err("missed FLOORPLAN name");
    }
  >>
  ;

history:
  TK_history (~SEMI)* SEMI
  ;

prop_def_section: TK_propertydefinitions
  property_defs TK_end TK_propertydefinitions
  ;

property_defs:
   (property_def)*
  ;

property_def:
          TK_design
          tstring>[sink] property_type_and_val SEMI 

        | TK_net
          tstring>[sink] property_type_and_val SEMI

        | TK_snet
          tstring>[sink] property_type_and_val SEMI

        | TK_region
          tstring>[sink] property_type_and_val SEMI

        | TK_group
          tstring>[sink] property_type_and_val SEMI

        | TK_component
          tstring>[sink] property_type_and_val SEMI

        | TK_row
          tstring>[sink] property_type_and_val SEMI

        | TK_pin
          tstring>[sink] property_type_and_val SEMI

        | TK_componentpin
          tstring>[sink] property_type_and_val SEMI
  ;


property_type_and_val:
    (TK_integer|TK_real) {range} {NUMBER}
  |  TK_string {QSTRING}
  |  TK_namemapstring tstring>[sink]
  ;


units:  TK_units TK_distance TK_microns i:NUMBER SEMI
  <<units_ . setDefMicrons(atoi($i->getText()));>>
  ;

divider_char: TK_dividerchar c:QSTRING SEMI
    <<def_dividerchar_ = strip_qstring(c->getText());>>
  ;

bus_bit_chars: TK_busbitchars c:QSTRING SEMI
    <<def_busbitchars_ = strip_qstring(c->getText());>>
  ;

site:
  <<SitePattern* sitep;>>
  TK_site site_body>[sitep]
  <<def_sites_.push_back(sitep);>>
  ;

canplace:
  <<SitePattern* sitep;>>
  TK_canplace site_body>[sitep]
  <<def_canplaces_.push_back(sitep);>>
  ;

cannotoccupy:
  <<SitePattern* sitep;>>
  TK_cannotoccupy site_body>[sitep]
  <<def_cannotoccupys_.push_back(sitep);>>
  ;


row_rule:
  <<Row* aRow; int site_index; StepPattern sp; ANTLRTokenPtr i,j;
    Unit ori_x,ori_y; int num_x,num_y; Unit step_x,step_y; int orient;
    char* row_name;>>

  TK_row tstring>[i] tstring>[j]
  <<site_index = (j != 0) ? sites_.find_index(text(j)) : -1;>>
      inumber>[ori_x] inumber>[ori_y]
      orientation>[orient]
      TK_do    inumber>[num_x] TK_by inumber>[num_y]
      TK_step  inumber>[step_x]      inumber>[step_y]
      <<sp = StepPattern(num_x,num_y,step_x,step_y);>>
      (row_option)* SEMI
  <<row_name = strdup((i != 0) ? text(i) : "");
    aRow = new Row(row_name, site_index,ori_x,ori_y,orient,sp);
    rows_.push_back(aRow);
  >>
  ;

row_option : PLUS TK_property (row_prop)*
  ;

row_prop : tstring>[sink] property_val
  ;



site_body > [SitePattern* sitep]:
  <<StepPattern sp; int site_index; ANTLRTokenPtr i;
    Unit ori_x,ori_y; int num_x,num_y; Unit step_x,step_y; int orient;>>
  tstring>[i] <<site_index = find_site_index(i);>>
  inumber>[ori_x] inumber>[ori_y] orientation>[orient]
  TK_do    inumber>[num_x] TK_by inumber>[num_y]
  TK_step  inumber>[step_x]      inumber>[step_y] SEMI
  <<sp = StepPattern(num_x,num_y,step_x,step_y);
    $sitep = new SitePattern( site_index, ori_x, ori_y, orient, sp);
  >>
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
  )
  ;


die_area:
  <<Pt sw,ne;>>
  TK_diearea pt>[sw] pt>[ne] SEMI
  <<die_area_.sw = sw; die_area_.ne = ne;>>
  ;

pin_cap_rule: start_def_cap (pin_cap)* end_def_cap
  ;

start_def_cap: TK_defaultcap NUMBER 
  ;


pin_cap: TK_minpins NUMBER TK_wirecap NUMBER SEMI
  ;


end_def_cap: TK_end TK_defaultcap 
  ;

pin_rule:
  <<int num_pins = 0, declared_pins; bool isNew;>>
  TK_pins inumber>[declared_pins] SEMI
  (pin>[isNew])*
  TK_end TK_pins
  ;

pin > [bool isNew]:
  <<
  ANTLRTokenPtr pin_id, net_id;
  //PinTuple tuple;
  Pin *aPin;
  int pin_index, net_index;
  Net *aNet; 
  >>
  MINUS tstring>[pin_id]
    <<//tuple = pins_.find_or_insert2(text(pin_id));
      pin_index = (pin_id != 0) ?
        static_cast<int>(pins_.find_or_insert_index(text(pin_id))) : -1;
      //aPin = tuple.first; $isNew = tuple.second;>>
      aPin = pins_[pin_index]; $isNew = pin_index == pins_.size() - 1;>>
  PLUS TK_net tstring>[net_id]
  <<net_index = (net_id != 0) ?
    static_cast<int>(nets_.find_or_insert_index(text(net_id))) : -1;
    aPin->setNet(net_index);
    aNet = nets_[net_index];
    aNet->addPin(pin_index);
  >>
  (pin_option<[aPin])* SEMI
  ;

pin_option < [Pin* aPin]:
  <<ANTLRTokenPtr layer_id; Pt p1,p2; int orient, layer_index;>>
  PLUS
  (   TK_special

  |   extension_stmt

  |   direction_spec<[aPin]

  |   TK_use use_option<[aPin]

  |   TK_layer tstring>[layer_id] pt>[p1] pt>[p2]
      <<layer_index = find_layer_index(layer_id);
        if (layer_index != -1) {
          aPin->setLayer(layer_index);
          aPin->setRect(Rect(p1,p2));
        }
      >>

  |   placement_status<[aPin] pt>[p1] orientation>[orient]
      <<aPin->setPoint(p1); aPin->setOrient(orient);>>

  )
  ;


direction_spec < [Pin* aPin]:
  TK_direction pin_direction<[aPin]
  ;

end_pins: TK_end TK_pins
  ;


tracks_rule:
  <<TrackPattern *tp; bool x_dir; int start,space,numTracks;>>
  TK_tracks
  ( TK_x <<x_dir = true;>> | TK_y <<x_dir=false;>> )
  inumber>[start]
  TK_do inumber>[numTracks] TK_step inumber>[space]
  <<tp = new TrackPattern(x_dir,start,numTracks,space);>>
  track_layer_statement<[tp] SEMI
  <<tracks_.push_back(tp);>>
  ;

track_layer_statement < [TrackPattern *trackpat]:
  <<int lno;>>
  {TK_layer (track_layer>[lno]
  <<if (lno != -1) {
      $trackpat->addLayer(lno);
    }
  >> )*}
  ;

track_layer > [int layer_no]:
  <<ANTLRTokenPtr i;>>
  tstring>[i] <<$layer_no = find_layer_index(i);>>
  ;

gcellgrid:
  TK_gcellgrid
  <<bool x_dir; Unit start, space; int num; >>
  ( TK_x <<x_dir = true;>> | TK_y <<x_dir=false;>> )
  inumber>[start] TK_do inumber>[num] TK_step inumber>[space] SEMI
  <<gcellpatterns_.push_back(new GcellPattern(x_dir,start,num,space));>>
  ;

extension_section: TK_beginext

  ;








extension_stmt: TK_beginext
  ;

via_section: via (via_declaration)* via_end
  ;
        
via: TK_vias NUMBER SEMI 
  ;


via_declaration:
  <<ANTLRTokenPtr via_name; std::pair<Via*,bool>  via_tie; Via *aVia;>>
  MINUS tstring> [via_name] <<via_tie = vias_.find_or_insert2(text(via_name));
                             aVia = via_tie.first;
                             if (!via_tie.second) { //
                               warn2(via_name, "Via %s redefinition",
                                    text(via_name));
                               aVia->destroy_geom();
                             } 
                             aVia->setDefVia();>>
        (layer_stmt<[aVia])* SEMI
   ;

layer_stmt < [Via *aVia]:
  <<ANTLRTokenPtr name; Pt p1,p2; std::vector<Rect>* rects = 0;
    int layer_index;>>
  PLUS  ( TK_rect tstring>[name] <<layer_index = find_layer_index(name);>>
           <<if (layer_index != -1)
               rects = & $aVia->prepareLayer(layer_index);>>
          pt>[p1] pt>[p2]
           <<if (rects) rects->push_back(Rect(p1,p2));>>

        | TK_patternname tstring>[name]
           <<$aVia->setPatternName(name->getText());>>

        | extension_stmt

        )
  ;

pt > [Pt point]:
  LPAREN (inumber>[pp.x]|STAR) <<$point.x=pp.x;>>
         (inumber>[pp.y]|STAR) <<$point.y=pp.y;>>
  RPAREN
  ;

via_end: TK_end TK_vias
  ;

regions_section:
  regions_start
  (regions_stmt)*
  TK_end TK_regions
  ;

regions_start: TK_regions NUMBER SEMI
  ;

regions_stmt:
  <<ANTLRTokenPtr regi_name; Region *aRegion;>>
  MINUS tstring>[ regi_name]
               <<aRegion = regions_.find_or_insert(text(regi_name));>>
  rect_list<    [aRegion] (region_option)* SEMI
  ;

rect_list < [Region *aRegion]:
  <<Pt p1,     p2;>>
  (pt>[p1] pt>[p2] <<$aRegion->addRegion(new Rect(p1,p2));>>)*
  ;

region_option : PLUS TK_property (region_prop)*
  ;
       
region_prop : tstring>[sink] property_val
  ;

comps_section:
  <<int num_comps=0, new_num_comps = 0, declared_comps; bool isNew;>>
  k:TK_components inumber>[declared_comps] SEMI
  (comp>[isNew] <<num_comps++; if (isNew) new_num_comps++;>>)*
  TK_end TK_components
  <<if (num_comps != declared_comps)
      warn2($k, "number of declared components %d differs from the real %d",
           declared_comps, num_comps);
  >>
  ;

comp > [bool isNew]:
  <<
  ComponentTuple tuple;
  ANTLRTokenPtr comp,mod,n;
  Component *aComp;
  int mod_index;
  >>
  comp_start>[tuple] <<aComp = tuple.first; $isNew = tuple.second;>>
  tstring>[mod]
  <<mod_index = find_macro_index(mod);
    aComp->setMacro(mod_index);>>
	(tstring>[n]
	|STAR           <<;>>
	)*

  (comp_option<[aComp])* SEMI
  ;


comp_start > [DEFParse::ComponentTuple tuple]:
  <<ANTLRTokenPtr comp;>>
  MINUS tstring>[comp] <<$tuple = components_.find_or_insert2(text(comp));>>
;

comp_option < [Component* aComp]:
    PLUS
    ( comp_generate | comp_source | comp_type<[aComp]
    | weight | comp_foreign | comp_region<[aComp] | comp_eeq
    | comp_property | comp_extension_stmt
    )
  ;

comp_extension_stmt: extension_stmt
  ;

comp_eeq: TK_eeqmaster <<;>> tstring>[sink]
  ;

comp_generate: TK_comp_gen <<;>> tstring>[sink]
    opt_pattern
  ;

opt_pattern : {tstring>[sink]}
  ;

comp_source: TK_source <<;>> tstring>[sink] 
  ;

comp_region < [Component* aComp]:
  <<ANTLRTokenPtr regi_name; Region& regi = aComp->getRegion(); int rind,cind;>>
	TK_region ( comp_pnt_list<[regi]
            | tstring>[regi_name]
              <<rind = find_region_index(regi_name);
                cind = find_component_index(regi_name, aComp->getName());
                region_contains_.insert(std::pair<int const,int>(rind,cind));
                aComp->setRegion(rind);>>
            )
  ;

comp_pnt_list < [Region& regi]:
  <<Pt  p1,     p2;>>
    pt>[p1] pt>[p2]  <<regi.addRegion(new Rect(p1,p2));>>
  ( pt>[p1] pt>[p2]  <<regi.addRegion(new Rect(p1,p2));>> )*
  ;

comp_property: TK_property
      (comp_prop)*
  ;

comp_prop: tstring>[sink] property_val
  ;

comp_foreign: TK_foreign tstring>[sink]
        opt_paren tstring>[sink]
  ;

opt_paren:
  <<Pt p;>>
         pt>[p]
       | NUMBER NUMBER
  ;

comp_type < [Component* aComp]:
  <<Pt p; ANTLRTokenPtr ori; int orient;>>
  placement_status<[aComp]
  pt>[p]          <<aComp->setPoint(p);>>
  orientation>[orient]<<aComp->setOrient(orient);>>
  //tstring>[ori]  <<aComp->setOri(convert_orientation(ori));>>
  ;

placement_status < [Placement* aPlc]:
        ( TK_fixed           <<aPlc->setType(Placement::FIXED);>>
        | TK_cover           <<aPlc->setType(Placement::COVER);>> 
        | TK_placed          <<aPlc->setType(Placement::PLACED);>>
        | TK_unplaced        <<aPlc->setType(Placement::UNPLACED);>>
        )
  ;

weight: TK_weight NUMBER 
  ;

nets_section:
  <<
  bool nets = true, isNew;
  int num_nets=0, new_num_nets=0, declared_nets;
  >>
  TK_nets inumber>[declared_nets] s:SEMI
  (one_net>[isNew] <<num_nets++; if (isNew) new_num_nets++;>>)*
  TK_end TK_nets 
  <<if (num_nets != declared_nets)
      warn2($s, "number of declared nets %d differs from the real %d",
           declared_nets, num_nets);
  >>
  ;

one_net > [bool isNew]:
  <<
  NetTuple tuple;
  Net *aNet;
  >>
  net_start>[tuple] <<aNet = tuple.first; aNet->setRegular();
                    $isNew = tuple.second;>>
  <<if (!$isNew) {aNet->destroyConn(Net::REGULAR);}>>
  (net_connection   <[false, aNet])*
  (net_option       <[aNet])* SEMI
  <<;>>
  ;

net_start > [DEFParse::NetTuple tuple]:
  <<ANTLRTokenPtr n;>>
  MINUS tstring>[n]
  <<$tuple = nets_.find_or_insert2(text(n));>> 
  ;

snet_start > [DEFParse::NetTuple tuple]:
  <<ANTLRTokenPtr n;>>
  MINUS tstring>[n]
  <<$tuple = nets_.find_or_insert2(text(n));>>
  ;

net_connection < [bool special, Net* aNet]:
  <<
  ANTLRTokenPtr comp,pin;
  Macro *aMacro = 0;
  int mod_index, comp_index, pin_index = 0;
  bool after_comp = false, after_star = false;
  >>
  ( lp:LPAREN
    ( <<LT(1)->getType()!=TK_pin>>?tstring>[comp]
      <<after_comp = true;
        comp_index = components_.find_index(text(comp));
        if (comp_index == -1) {
          db::err2(comp,"undefined component name");
        } else {
          mod_index = components_[comp_index]->getMacro();
          //assert(mod_index >= 0 && mod_index < macros_.size());
          if (!(mod_index >= 0 && mod_index < macros_.size())) {
            std::cout << "comp : " << text(comp)
                      << " comp_index = " << comp_index
                      << " mod_index  = " << mod_index << std::endl;
            throw std::exception();
            std::exit(1);
          } else {
            aMacro = macros_[mod_index];
          }
        }
      >>

    | STAR   <<after_star = true;>>

    | TK_pin <<;>>

    )
    (tstring>[pin] {conn_opt})
    RPAREN
    <<if (after_comp) {
        pin_index = (aMacro ? aMacro->getMacroPinIndex(text(pin)) : -1);
        if (pin_index != -1) {
          aNet->addConn(special, comp_index, pin_index);
        } else {
          db::err2(comp,"undefined pin name for macro %s",
                aMacro ? aMacro->getName() : "???");
        }
      }
      if (after_star) {
        for (int k = 0; k < components_.size(); k++) {
          mod_index = components_[k]->getMacro();
          assert(mod_index >= 0 && mod_index < macros_.size());
          aMacro = macros_[mod_index];
          pin_index = (aMacro ? aMacro->getMacroPinIndex(text(pin)) : -1);
          if (pin_index != -1) {
            aNet->addConn(special, k, pin_index);
          }
        }
      }
    >>

  )

  | TK_mustjoin LPAREN tstring>[sink] <<;>> tstring>[sink]
    {conn_opt} RPAREN

  ;

conn_opt:
          extension_stmt

        | PLUS TK_synthesized
	  <<;>>
  ;

net_option < [Net* aNet]:
  <<NetType::Class t; RouteContainer* route; NetUse* nuse = &aNet->regUse();>>
  PLUS
  ( net_type>[t]  <<route = &aNet->getRegRoute();>>
    paths<[route,t]
  | TK_source <<;>> tstring>[sink]
          <<;>>
  | TK_original <<;>> tstring>[sink]
          <<;>>

  | TK_pattern <<;>> tstring>[sink]
          <<;>>

  | TK_weight NUMBER
          <<;>>

  | TK_xtalk NUMBER
          <<;>>

  | TK_estcap NUMBER
          <<;>>

  | TK_use use_option<[nuse]

  | TK_style <<;>> tstring>[sink]
          <<;>>

  | TK_nondefaultrule <<;>> tstring>[sink]
          <<;>>

  |    vpin_stmt

  | TK_shieldnet <<;>> tstring>[sink]
          <<;>>

  | TK_noshield paths<[0]

  | TK_subnet tstring>[sink] (comp_name)* (subnet_option)*


  | TK_property <<;>> (net_prop)*
          <<;>>

  | extension_stmt
          <<;>>
  )
  ;

pin_direction < [Pin* aPin]
    : TK_input    <<aPin->setDirection(Pin::INPUT);>>
    | TK_inout    <<aPin->setDirection(Pin::INOUT);>>
    | TK_output   <<aPin->setDirection(Pin::OUTPUT);>>    
    | TK_feedthru <<aPin->setDirection(Pin::FEEDTHRU);>>
    ;

use_option < [NetUse* use]:
    ( TK_signal   <<use->setUse(NetUse::SIGNAL);>>
    | TK_power    <<use->setUse(NetUse::POWER); >>
    | TK_ground   <<use->setUse(NetUse::GROUND);>>
    | TK_clock    <<use->setUse(NetUse::CLOCK); >>
    | TK_tieoff   <<use->setUse(NetUse::TIEOFF);>>
    | TK_analog   <<use->setUse(NetUse::ANALOG);>>
    | T_STRING
    ) 
  ;


net_prop: tstring>[sink] property_val
  ;

vpin_stmt:
  <<Pt p1,p2;>>
  vpin_begin {vpin_layer} pt>[p1] pt>[p2] vpin_options
  ;

vpin_begin: TK_vpin <<;>> tstring>[sink]
	  <<;>>
  ;

vpin_layer:
  TK_layer <<;>> tstring>[sink]
	   <<;>>
  ;

property_val: NUMBER

        | QSTRING
            <<;>>
  ;

vpin_options:
  <<Pt p; int orient;>>
        (vpin_status pt>[p] orientation>[orient])*
  ;

vpin_status:
          TK_placed 
        | TK_fixed 
        | TK_cover
  ;

net_type > [NetType::Class type]:
  ( TK_fixed   <<$type = NetType::FIXED;>>
  | TK_cover   <<$type = NetType::COVER;>>
  | TK_routed  <<$type = NetType::ROUTED;>>
  )
  ;

paths < [RouteContainer* route, NetType::Class type = NetType::ROUTED]:
  <<RouteObject robj,*ptr = 0;>>
  <<if (route) {
        route->push_back(robj);
        ptr = &route->back();
        ptr->setType(type);} >>
  path<[ptr]
  (new_path<[route,type])*
  ;

new_path < [RouteContainer* route, NetType::Class type = NetType::ROUTED]:
  <<RouteObject robj,*ptr = 0;>>
  TK_new
  <<if (route) {
        route->push_back(robj);
        ptr = &route->back();
        ptr->setType(type);} >>
  path<[ptr]
  ;

path < [RouteObject* robj]:
  <<ANTLRTokenPtr tok; Unit w = 0; Pt p;
    int layer,value,taper_index = -1;
    RouteObject::Shape s = RouteObject::UNKNOWN;
    bool new_pt;>>
  tstring>[tok] <<layer = find_layer_index(tok);
                  if (layer == -1) robj = 0; // disable updating robj
                  if (robj) robj->setLayer(layer);>>
  {width>[w]    <<if (robj) robj->setWidth(w);>>}
  {shape>[s]    <<if (robj) robj->setShape(s);>>}
  {taper>[taper_index]     <<if (robj) robj->setTaper(taper_index);>>}
  path_pt>[p,value,new_pt] <<if (robj) robj->addPoint(p); >>
  (path_item<[robj])*
  ;

path_item < [RouteObject* robj]:
  <<Pt p; int value; bool new_pt;>>
    ( t:T_STRING
      <<if (robj) robj->addPoint(find_via_index($t));>>
    | path_pt>[p,value,new_pt]
      <<if (robj) robj->addPoint(p); >>
    )
  ;

path_pt > [Pt point, int value, bool new_pt]:
  <<$value = -1; $new_pt = true;>>
  LPAREN (inumber>[pp.x]|STAR <<$new_pt = false;>>) <<$point.x=pp.x;>>
         (inumber>[pp.y]|STAR <<$new_pt = false;>>) <<$point.y=pp.y;>>
         {inumber>[$value]}
  RPAREN
  ;

taper > [int taper_index]:
  <<ANTLRTokenPtr tok;>>
  ( TK_taper                   <<$taper_index = -1;>>
  | TK_taperrule tstring>[tok] <<$taper_index = find_nrule_index(tok);>>
  )
  ;

shape > [RouteObject::Shape s]:
  PLUS TK_shape
  ( TK_ring                    <<$s = RouteObject::RING;         >>
  | TK_stripe                  <<$s = RouteObject::STRIPE;       >>
  | TK_followpin               <<$s = RouteObject::FOLLOWPIN;    >>
  | TK_iowire                  <<$s = RouteObject::IOWIRE;       >>
  | TK_blockwire               <<$s = RouteObject::BLOCKWIRE;    >>
  | TK_corewire                <<$s = RouteObject::COREWIRE;     >>
  | TK_fillwire                <<$s = RouteObject::FILLWIRE;     >>
  | TK_blockagewire            <<$s = RouteObject::BLOCKAGEWIRE; >>
  )
  ;

width > [Unit w]:
  inumber>[$w]
  ;

snets_section :
  <<bool isNew;>>
  start_snets
  (snet_rule>[isNew])*
  end_snets

  ;

snet_rule > [bool isNew]:
  <<
  NetTuple tuple;
  Net *aNet;
  >>
  snet_start>[tuple] <<aNet = tuple.first; aNet->setSpecial();
                     $isNew = tuple.second;>>
  <<if (!$isNew) {aNet->destroyConn(Net::SPECIAL);}>>
  (net_connection    <[true, aNet])*
  (snet_option       <[aNet])* SEMI
  <<;>>
  ;


snet_option < [Net* aNet]: PLUS
  ( snet_width   | snet_voltage |
  | snet_spacing | snet_other_option<[aNet]
  )
  ;

snet_other_option < [Net* aNet]:
  <<NetType::Class t; RouteContainer* route; NetUse* nuse = &aNet->specUse();>>
        ( net_type>[t]  <<route = &aNet->getSpecRoute();>>
          paths<[route,t]
 
        | TK_shield <<;>> tstring>[sink]
          paths<[0]
 
        | TK_source <<;>> tstring>[sink]
          <<;>>
 
        | TK_original <<;>> tstring>[sink]
          <<;>>
 
        | TK_pattern <<;>> tstring>[sink]
          <<;>>
 
        | TK_weight NUMBER
          <<;>>
 
        | TK_estcap NUMBER
          <<;>>
 
        | TK_use use_option<[nuse]

        | TK_style <<;>> tstring>[sink]
          <<;>>
 
        | TK_property <<;>> (net_prop)*
          <<;>>
 
        | extension_stmt
          <<;>>
        )
  ;

snet_width:   TK_width <<;>> tstring>[sink] NUMBER
  ;

snet_voltage: TK_voltage NUMBER
  ;

snet_spacing: TK_spacing <<;>> tstring>[sink] NUMBER
        {snet_range}
  ;

snet_range:
        TK_range NUMBER NUMBER
  ;

range:
        TK_range NUMBER NUMBER
            <<;>>
  ;

start_snets: TK_snets NUMBER SEMI
      <<;>>
  ;

end_snets: TK_end TK_snets 
	  <<;>>
  ;

groups_section: groups_start (group_rule)* groups_end
  ;

groups_start: TK_groups NUMBER SEMI
	<<;>>
  ;

group_rule:
  <<Group*        aGroup;>>
  start_group   >[aGroup]
  (group_member <[aGroup])*
  (group_option <[aGroup])*
  SEMI
	<<;>>
  ;

start_group >    [Group* aGroup]:
  <<ANTLRTokenPtr group_name;>>
  MINUS tstring>[ group_name]
              <<$aGroup = groups_.find_or_insert(text(group_name));>>
  ;

group_member <   [Group* aGroup]:
  <<ANTLRTokenPtr comp_name;>>
  tstring>[comp_name]
  <<aGroup->addComp(find_component_index(comp_name));>>
  ;

group_option <   [Group* aGroup]:
  PLUS
        ( TK_soft (group_soft_option)*
	  
        | TK_property <<;>> (group_prop)*
	  
        | TK_region <<;>> group_region<[aGroup]
	  
        | extension_stmt
        )
  ;

group_region < [Group* aGroup]:
  <<ANTLRTokenPtr regi_name; Pt p1,p2; Region& regi = aGroup->getRegion();>>
     pt>[p1] pt>[p2]     <<regi.addRegion(new Rect(p1,p2));>>
   | tstring>[regi_name] <<aGroup->setRegion(find_region_index(regi_name));>>
  ;

group_prop : tstring>[sink] property_val
        <<;>>
  ;

group_soft_option:
           TK_maxx NUMBER

         | TK_maxy NUMBER

         | TK_maxhalfperimeter NUMBER
  ;

groups_end: TK_end TK_groups
  ;

assertions_section: assertions_start (constraint_rule)* assertions_end
  ;

constraint_section: constraints_start (constraint_rule)* constraints_end
  ;

assertions_start: TK_assertions NUMBER SEMI
  ;

constraints_start: TK_constraints NUMBER SEMI
  ;

constraint_rule: MINUS
  ( operand_rule 
  | wiredlogic_rule
  )
  ;


operand_rule: operand (delay_spec)* SEMI
  ;

operand:  TK_net  tstring>[sink] 

        | TK_path tstring>[sink] tstring>[sink] tstring>[sink] tstring>[sink]

        | TK_sum  LPAREN (operand)* RPAREN

        | TK_diff LPAREN (operand)* RPAREN
  ;


wiredlogic_rule: TK_wiredlogic tstring>[sink]
      {PLUS} TK_maxdist NUMBER SEMI
  ;


delay_spec: PLUS
        ( TK_risemin NUMBER 

        | TK_risemax NUMBER 

        | TK_fallmin NUMBER 

        | TK_fallmax NUMBER 

        )
  ;

constraints_end: TK_end TK_constraints
  ;

assertions_end: TK_end TK_assertions
  ;

scanchains_section: scanchain_start (scan_rule)* scanchain_end
            ;

scanchain_start: TK_scanchains NUMBER SEMI
  ;

scan_rule: start_scan (scan_member)* SEMI 
  ;

start_scan: MINUS tstring>[sink] 
  ;

opt_pin : {tstring>[sink]}
  ;

scan_member: PLUS
        ( TK_start <<;>> tstring>[sink] opt_pin

        | TK_floating <<;>> (floating_inst)*
	  <<;>>

        | TK_ordered
          ordered_inst_list
	  <<;>>

        | TK_stop <<;>> tstring>[sink] opt_pin


	| TK_commonscanpins <<;>>
	  opt_common_pins
	  <<;>>

        | extension_stmt
        )
  ;

opt_common_pins:
    (LPAREN tstring>[sink] tstring>[sink] RPAREN)*
  ;


floating_inst:
  tstring>[sink] floating_pins
      <<;>>
  ;

floating_pins:
  (LPAREN tstring>[sink] tstring>[sink] RPAREN)*
  ;
  
ordered_inst_list:
  (one_ordered_inst)*
  ;

one_ordered_inst: tstring>[sink]
    ordered_pins
      <<;>>
  ;

ordered_pins:
  (LPAREN tstring>[sink] tstring>[sink] RPAREN)*
  ;

scanchain_end: TK_end TK_scanchains
	<<;>>
  ;

iotiming_section: iotiming_start (iotiming_rule)* iotiming_end
  ;

iotiming_start: TK_iotimings NUMBER SEMI
  ;

iotiming_rule: start_iotiming (iotiming_member)* SEMI
  ;

start_iotiming: MINUS {LPAREN <<;>> tstring>[sink] tstring>[sink] RPAREN}
  ;

iotiming_member:
        ( PLUS
          (  risefall (TK_variable|TK_slewrate) NUMBER NUMBER

          |  TK_capacitance NUMBER

          |  TK_drivecell <<;>> tstring>[sink]
          )
        )
        (

        |  TK_frompin   <<;>> tstring>[sink]

        |  TK_topin     <<;>> tstring>[sink]

        |  TK_parallel NUMBER

        |  extension_stmt

        )
  ;

risefall: TK_rise <<;>> | TK_fall <<;>>
  ;

iotiming_end: TK_end TK_iotimings
	<<;>>
  ;

floorplan_contraints_section: fp_start (fp_stmt)* TK_end TK_fpc
          <<;>>
  ;

fp_start: TK_fpc NUMBER SEMI
  ;

fp_stmt: MINUS <<;>> tstring>[sink] h_or_v
          <<;>>
          constraint_type (constrain_what)* SEMI
          <<;>>
  ;

h_or_v:   TK_horizontal 
            <<;>>
        | TK_vertical
            <<;>>
  ;

constraint_type:
          TK_align
            <<;>>
        | TK_max NUMBER
            <<;>>
        | TK_min NUMBER
            <<;>>
        | TK_equal NUMBER
            <<;>>
  ;


constrain_what: PLUS
        (   TK_bottomleft
            <<;>>
            row_or_comp_list
        |   TK_topright
            <<;>>
            row_or_comp_list
        )
        ;

row_or_comp_list:
  (row_or_comp)*
  ;

row_or_comp: LPAREN ( TK_rows | TK_comps )  tstring>[sink] RPAREN
  ;

timingdisables_section: timingdisables_start (timingdisables_rule)* timingdisables_end
  ;

timingdisables_start: TK_timingdisables NUMBER SEMI
  ;

timingdisables_rule: MINUS
        ( TK_frompin <<;>> tstring>[sink] tstring>[sink]
            	     	  TK_topin <<;>> tstring>[sink] tstring>[sink] SEMI

        | TK_thrupin <<;>> tstring>[sink] tstring>[sink] SEMI

        | TK_macro <<;>>   tstring>[sink] td_macro_option SEMI

        | TK_reentrantpaths SEMI
        )
  ;


td_macro_option: TK_frompin <<;>> tstring>[sink] TK_topin <<;>> tstring>[sink]

        |        TK_thrupin <<;>> tstring>[sink]
  ;

timingdisables_end: TK_end TK_timingdisables
      <<;>>
  ;

partitions_section: partitions_start (partition_rule)* partitions_end
  ;

partitions_start: TK_partitions NUMBER SEMI
  ;


partition_rule: start_partition (partition_member)* SEMI 
  ;

start_partition: MINUS tstring>[sink] {turnoff}
  ;

turnoff: TK_turnoff {turnoff_setup} {turnoff_hold}
  ;

turnoff_setup:
          TK_setuprise
            <<;>>
        | TK_setupfall
            <<;>>
  ;

turnoff_hold:
          TK_holdrise
            <<;>>
        | TK_holdfall
            <<;>>
  ;

partition_member: PLUS
        (   TK_fromclockpin <<;>>
            tstring>[sink] tstring>[sink] risefall minmaxpins

        |   TK_fromcomppin <<;>>
	    tstring>[sink] tstring>[sink] (risefallminmax2)*

        |   TK_fromiopin <<;>> tstring>[sink]
	    (risefallminmax1)*

        |   TK_toclockpin <<;>>
	    tstring>[sink] tstring>[sink] risefall minmaxpins

        |   TK_tocomppin <<;>>
	    tstring>[sink] tstring>[sink] (risefallminmax2)*

        |   TK_toiopin <<;>> tstring>[sink] (risefallminmax1)*

        |   extension_stmt
        )
  ;

minmaxpins: (min_or_max_member)* TK_pins
     <<;>> pin_list
        <<;>>
  ;


min_or_max_member:
          TK_min NUMBER NUMBER

        | TK_max NUMBER NUMBER
  ;

pin_list:
  (tstring>[sink])*
  <<;>>
  ;

risefallminmax1:
          TK_risemin NUMBER
    <<;>>
        | TK_fallmin NUMBER
    <<;>>
        | TK_risemax NUMBER
    <<;>>
        | TK_fallmax NUMBER
    <<;>>
  ;

risefallminmax2:
          TK_risemin NUMBER NUMBER

        | TK_fallmin NUMBER NUMBER

        | TK_risemax NUMBER NUMBER

        | TK_fallmax NUMBER NUMBER
  ;

partitions_end: TK_end TK_partitions
  ;

comp_name: LPAREN tstring>[sink]
  tstring>[sink] {PLUS TK_synthesized} RPAREN
  ;

subnet_option:
  <<NetType::Class t;>>
  ( net_type>[t] paths<[0]
  | TK_nondefaultrule tstring>[sink]
  )
  ;

pin_props_section: begin_pin_props pin_prop_list end_pin_props ;

begin_pin_props: TK_pinproperties NUMBER {SEMI}
  ;

end_pin_props: TK_end TK_pinproperties
  ;

pin_prop_list:
    (pin_prop_terminal)*
    ;

pin_prop_terminal: MINUS tstring>[sink] tstring>[sink]
    (pin_prop)* SEMI
  ;

pin_prop: PLUS TK_property
   (pin_prop_name_value)*
  ;

pin_prop_name_value : tstring>[sink] property_val
  ;


// BLOCKAGES 5.5 extension

blockages_section:
  <<ANTLRTokenPtr i,j; int layer_no,comp_index; Pt p1,p2; Blockage *bl = 0;>>
  TK_blockages NUMBER SEMI
    (MINUS
       (
         ( TK_layer tstring>[i]
          <<layer_no = find_layer_index(i);
            if (layer_no != -1) {
              bl = new Blockage(layer_no);
            }
          >>
          (PLUS ( TK_component tstring>[j]
                <<comp_index = find_component_index(j);
                  if (bl && comp_index != -1) {
                    bl->setComp(comp_index);
                  }
                >>
                | TK_slots    <<if (bl) bl->setSlots();>>
                | TK_fills    <<if (bl) bl->setFills();>>
                | TK_pushdown <<if (bl) bl->setPushdown();>>
                )
          )*
          (TK_rect pt>[p1] pt>[p2] <<if (bl) bl->addRect(Rect(p1,p2));>>)*
          <<if (bl) blockages_.push_back(bl); bl = 0;>>

         | TK_placement <<bl = new Blockage();>>
          (PLUS ( TK_component tstring>[j]
                <<comp_index = find_component_index(j);
                  if (bl && comp_index != -1) {
                    bl->setComp(comp_index);
                  }
                >>
                | TK_pushdown <<if (bl) bl->setPushdown();>>
                )
          )*
          (TK_rect pt>[p1] pt>[p2] <<if (bl) bl->addRect(Rect(p1,p2));>>)*
          <<if (bl) blockages_.push_back(bl); bl = 0;>>
         )
     SEMI )* // MINUS
    )*
  TK_end TK_blockages
  ;


number > [Rat num] :
  n:NUMBER
    <<$num = Rat(n->getText());>>
  ;


inumber > [int n] :
  <<Rat r;>>
  t:NUMBER
  <<r = Rat(t->getText());
   if (r.den() > 1) {warn2(t," expected integer (float truncated)");}
   $n = int(r.num());>>
  ;

tstring > [ANTLRTokenPtr tok] :
  ( t1:T_STRING
    <<$tok = $t1;
      if (strchr($t1->getText(),'*')
       || strchr($t1->getText(),'%') ) {
        regexp_seen = true;
      }
    >>
  | t2:TK_version .. TK_noshield  <<$tok = $t2; regexp_seen = false;>>
  | t3:NUMBER                     <<$tok = $t3; regexp_seen = false;>>
  )
  ;

//tstring1 > [ANTLRTokenPtr tok] :
//  ( t1:T_STRING             <<$tok = $t1;>>
//  | t2:TK_version     .. TK_xtalk     <<$tok = $t2;>>
//  | t3:TK_synthesized .. TK_noshield  <<$tok = $t3;>>
//  )
//  ;

//#token TK_xtalk
//#token TK_pin  		  
//#token TK_synthesized
