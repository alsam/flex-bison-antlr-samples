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
//  SPEF grammar parser using PCCTS.
//
// $Id: SPEFparse.g,v 1.1.4.3 2003/02/12 12:40:49 flying Exp $
//

#header <<
  #include "design_interface/db_support.h"
  #include "design_interface/parser.h"

  #define PURIFY(r,s)
  #define PCCTS_PURIFY(r,s)

  using db::DBSupport;

  #include <map>
  #include <string>
>>

<<
  #include <cstdlib> // atoi(), atof()
  #include <cstring>

  #ifdef SPEF_DEBUG
  #include <iostream>
  #endif

  typedef ANTLRCommonToken ANTLRToken;

>>

<<

static char * mystrdup(const char *s)
{
  size_t size = strlen(s) + 1;
  char *buf = (char *) malloc(size);
  memcpy(buf, s, size);
  return buf;
}


void SPEFParse::strip_path(std::string* p)
{
  std::string& s = *p;
  std::string::size_type l = s.length();

  if (l > 0 && s[l-1] == hier_delim_ && (l == 1 || s[l-2] != '\\'))
    s.erase(l-1);
  if (s[0] == hier_delim_)
    s.erase(0, 1);
}

void SPEFParse::split_triplet(const std::string& s,
                              double *d0, double *d1, double *d2)
{
  char *p = (char *) s.c_str(); // strtod() requires char** as second arg.
  char *q = p;

  *d0 = strtod(p, &q);
  if (q == p) {
    db::warn("triplet expected, got `%s'", s.c_str());
    return;
  }
  if (*q == ':') {
    *d1 = strtod(++q, &p);
    if (p != q && *p == ':') {
      *d2 = strtod(++p, &q);
      if (q == p)
        db::warn("triplet expected, got `%s'", s.c_str());
    } else
      db::warn("triplet expected, got `%s'", s.c_str());
  } else
    *d2 = *d1 = *d0;
}


const std::string SPEFParse::index_to_name(int ind)
{
  NameMapConn::iterator i = NameMap_.find(ind);
  if (i != NameMap_.end()) {
    return i->second;
  } else {
    db::warn("Index `*%d' was not found in NAME_MAP", ind);
    return "";
  }
}

inline
const std::string SPEFParse::index_to_name(const std::string& s)
{
  if (s[0] == '*')
    return index_to_name(atoi(s.c_str() + 1)); // Adds 1 to skip over '*'.

  return s;
}

void SPEFParse::name_info(const std::string& entity,
                          std::string *inst, std::string *pin)
{
  std::string::size_type p = entity.rfind(pin_delim_);

  if (p == 0) {
    db::warn("`%s' should not begin with pin_delim `%c'",
          entity.c_str(), pin_delim_);
    p = std::string::npos;
  }

  if (p != std::string::npos) {
    *inst = entity.substr(0, p);
    *pin = entity.substr(p + 1);
  } else {
    *inst = ""; // FIXME: some default inst name here?
    *pin = entity;
  }

  *inst = index_to_name(*inst);
  strip_path(inst);

  *pin = index_to_name(*pin);
}


bool SPEFParse::find_port(const std::string& sinst, const std::string& spin,
                          int *inst, int *pin)
{
  *inst = components_.find_index(sinst.c_str());
  if (*inst == -1)
    return false;

  Macro *macro = macros_[components_[*inst]->getMacroIndex()];
  *pin = macro->getMacroPinIndex(spin.c_str());
  if (*pin == -1)
    db::warn("Pin `%s' was not found", spin.c_str());
    // But return true since port was found.

  *inst++;
  *pin++;
  return true;
}


bool SPEFParse::find_int_node(const std::string& sinst,
                              const std::string& spin, int *inst, int *pin)
{
  *inst = nets_.find_index(sinst.c_str());
  if (*inst == -1)
    return false;

  if (*inst != current_net_->net_index)
    db::warn("Referencing internal nodes of other nets is unsupported");

  if (spin.find_first_not_of("0123456789") != std::string::npos)
    db::err("Pin `%s' should be pos_integer", spin.c_str());

  *pin = atoi(spin.c_str());

  *inst++;
  return true;
}


bool SPEFParse::find_int_conn(const std::string& sinst,
                              const std::string& spin, int *inst, int *pin)
{
  *inst = components_.find_index(sinst.c_str());
  if (*inst == -1)
    return false;

  Macro *macro = macros_[components_[*inst]->getMacroIndex()];
  *pin = macro->getMacroPinIndex(spin.c_str());
  if (*pin == -1)
    db::warn("Pin `%s' was not found", spin.c_str());
    // But return true since port was found.
 
  t_spef_d_net::PinConn& pins =
    static_cast<t_spef_d_net *>(current_net_)->pins;
  t_spef_d_net::PinConn::iterator i = pins.find(std::make_pair(*inst, *pin));
  if (i == pins.end())
    db::warn("Component `%s%c%s' should be defined in *CONN section first",
         sinst.c_str(), pin_delim_, spin.c_str());

  *inst++;
  *pin++;
  return true;
}


bool SPEFParse::find_pin_name(const std::string& sinst,
                              const std::string& spin, int *inst, int *pin)
{
  *inst = components_.find_index(sinst.c_str());
  if (*inst == -1)
    return false;

  Macro *macro = macros_[components_[*inst]->getMacroIndex()];
  *pin = macro->getMacroPinIndex(spin.c_str());
  if (*pin == -1)
    db::warn("Pin `%s' was not found", spin.c_str());
    // But return true since port was found.

  *inst++;
  *pin++;
  return true;
}


bool SPEFParse::find_net(const std::string& net, int *net_index)
{
  *net_index = nets_.find_index(net.c_str());

  return (*net_index++ != -1);
}


void SPEFParse::parse_design_flow(const char *qs)
{
  char *const buf = mystrdup(qs);
  char *s = buf;
  ++s;                          // Skip opening quote mark.
  *(s + strlen(s) - 1) = 0;     // Erase closing quote mark.
  s += strspn(s, " \t");

  size_t len = strcspn(s, " \t");
  if (len == 0)
    return;

  *(s + len) = 0;

  DesignFlowConn::iterator i = DesignFlow_.find(s);
  if (i != DesignFlow_.end())
    (this->*i->second)(s, s + len + 1);

  free(buf);
}


void SPEFParse::routing_confidence(const char *cmd, char *arg)
{
  char *p;
  conf_ = strtol(arg, &p, 10);
  if (p == arg || conf_ < 0 || *(p + strspn(p, " \t")) != 0) {
    db::warn("`%s' should be followed by pos_integer, got `%s'", cmd, arg);
    conf_ = -1;
  }
}


void SPEFParse::slew_thresholds(const char *cmd, char *arg)
{
  split_triplet(arg, &thr_r_[0],
                &thr_r_[1], &thr_r_[2]);

  arg += strspn(arg, " \t");
  arg += strspn(arg, ".0123456789:");

  split_triplet(arg, &thr_f_[0],
                &thr_f_[1], &thr_f_[2]);
}


void SPEFParse::init()
{
  DesignFlow_["ROUTING_CONFIDENCE"] = &SPEFParse::routing_confidence;
  DesignFlow_["SLEW_THRESHOLDS"] = &SPEFParse::slew_thresholds;
  conf_ = -1;
  thr_r_[0] = thr_r_[1] = thr_r_[2] =
    thr_f_[0] = thr_f_[1] = thr_f_[2] = -1;
}

>>

class SPEFParse : public DBSupport
{

<<
private:
  void strip_path(std::string* s);
  void split_triplet(const std::string& s,
                     double *d0, double *d1, double *d2);
  const std::string index_to_name(int ind);
  inline const std::string index_to_name(const std::string& s);

  bool find_port(const std::string& sinst, const std::string& spin,
                 int *inst, int *pin);
  bool find_int_node(const std::string& sinst, const std::string& spin,
                     int *inst, int *pin);
  bool find_int_conn(const std::string& sinst, const std::string& spin,
                     int *inst, int *pin);
  bool find_pin_name(const std::string& sinst, const std::string& spin,
                     int *inst, int *pin);
  bool find_net(const std::string& net, int *net_index);
  
  void parse_design_flow(const char *qs);
  void routing_confidence(const char *cmd, char *arg);
  void slew_thresholds(const char *cmd, char *arg);

protected:
  typedef std::map<int, std::string> NameMapConn;
  NameMapConn NameMap_;

  typedef std::map<std::string,
                   void (SPEFParse::*)(const char *cmd, char *arg)>
    DesignFlowConn;
  DesignFlowConn DesignFlow_;

  typedef std::map<std::pair<int, int>, t_spef_port *> PortConn;
  PortConn ports_;

  char pin_delim_, hier_delim_, pb_delim_, sb_delim_;

  double time_scale_, cap_scale_, res_scale_, induc_scale_;

  int conf_;
  double thr_r_[3], thr_f_[3];

  t_spef_net *current_net_;

  void init();
  void name_info(const std::string& entity,
                 std::string *inst, std::string *pin);

>>

#token TK_spef
#token TK_design
#token TK_date
#token TK_vendor
#token TK_program
#token TK_version
#token TK_design_flow
#token TK_divider
#token TK_delimiter
#token TK_bus_delimiter

#token TK_t_unit
#token TK_c_unit
#token TK_r_unit
#token TK_l_unit

#token TK_name_map
#token TK_power_nets
#token TK_ground_nets

#token TK_ports
#token TK_physical_ports

#token TK_c
#token TK_l
#token TK_s
#token TK_d

#token TK_define
#token TK_pdefine

#token TK_d_net
#token TK_r_net
#token TK_d_pnet
#token TK_r_pnet

#token TK_end

#token TK_v

#token TK_conn
#token TK_cap
#token TK_res
#token TK_induc

#token TK_p
#token TK_i
#token TK_n

#token TK_driver
#token TK_cell
#token TK_c2_r1_c1
#token TK_loads

#token TK_rc
#token TK_q
#token TK_k

#token DOT
#token SLASH
#token BAR
#token LPAREN
#token RPAREN
#token LBRACKET
#token RBRACKET
#token LBRACE
#token RBRACE
#token LANGEL
#token RANGEL
#token COLON

#token POS_INTEGER
#token POS_FRACTION
#token POS_FLOAT
#token FLOAT
#token NUMBER

#token PF_TRIPLET
#token PV_TRIPLET

#token DIRECTION
#token TIME_UNIT
#token CAP_UNIT 
#token RES_UNIT 
#token INDUC_UNIT

#token INDEX
#token IDENTIFIER
#token QSTRING
#token BIT_IDENTIFIER
#token PHYSICAL_REF
#token PATH

#token PATH_PREF_DELIM

#token Eof                  "@"

#tokclass HCHAR_            { DOT SLASH COLON BAR }
#tokclass PB_DELIM_         { LBRACKET LBRACE LPAREN LANGEL COLON DOT }
#tokclass SB_DELIM_         { RBRACKET RBRACE RPAREN RANGEL }
#tokclass IDENT_            { IDENTIFIER POS_INTEGER POS_FLOAT // Like 12e6
                              DIRECTION TIME_UNIT CAP_UNIT RES_UNIT
                              INDUC_UNIT }
#tokclass NAME_             { IDENT_ QSTRING }
#tokclass BIT_IDENTIFIER_   { IDENT_ BIT_IDENTIFIER POS_FLOAT } // Like 12.6

#tokclass FLOAT_            { POS_FRACTION POS_FLOAT FLOAT }
#tokclass POS_NUMBER_       { POS_INTEGER POS_FRACTION POS_FLOAT }
#tokclass NUMBER_           { NUMBER FLOAT_ POS_NUMBER_ }

#tokclass PHYSICAL_REF_     { NAME_ PHYSICAL_REF BIT_IDENTIFIER_ PV_TRIPLET }
#tokclass PATH_             { POS_FRACTION BIT_IDENTIFIER_ PV_TRIPLET
                              PATH PHYSICAL_REF }
#tokclass PATH2_            { BIT_IDENTIFIER_ PV_TRIPLET PATH PHYSICAL_REF }

#tokclass PORT_NAME_        { PATH_PREF_DELIM PATH2_ INDEX }
#tokclass PPORT_NAME_       { PATH_PREF_DELIM PHYSICAL_REF_ INDEX }
#tokclass PIN_NAME_         { PATH_PREF_DELIM PATH2_ }
#tokclass PNODE_REF_        { PATH_PREF_DELIM PHYSICAL_REF_ }
#tokclass INT_NODE_NAME_    { PATH_PREF_DELIM PATH2_ }
#tokclass INT_PNODE_NAME_   { PATH_PREF_DELIM PHYSICAL_REF_ }

#tokclass PF_TRIPLET_       { PF_TRIPLET POS_FRACTION }
#tokclass PV_TRIPLET_       { PV_TRIPLET PF_TRIPLET_ FLOAT_ }

start_translation:
  <<;>>
  << init(); >>
  spef_file
  Eof
  <<
     for (PortConn::iterator i = ports_.begin(); i != ports_.end(); i++)
       spef_ports_.push_back(i->second);

     #ifdef SPEF_DEBUG

     for_each(spef_ports_.begin(), spef_ports_.end(),
              deref_print<t_spef_port *>(std::cout));

     for_each(spef_nets_.begin(), spef_nets_.end(),
              deref_print<t_spef_net *>(std::cout));

     #endif /* SPEF_DEBUG */
  >>
  ;

spef_file:
  header_def {name_map} {power_def} {external_def} {define_def} internal_def
  ;

// 9.3.2.2 Header definition
header_def:
  spef_version design_name date vendor program_name program_version
  design_flow hierarchy_div_def pin_delim_def bus_delim_def unit_def
  ;

spef_version:
  TK_spef qs:QSTRING
  << spef_header_.spef_version = text($qs); >>
  ;

design_name:
  TK_design qs:QSTRING
  << spef_header_.design = text($qs); >>
  ;

date:
  TK_date qs:QSTRING
  << spef_header_.date = text($qs); >>
  ;

vendor:
  TK_vendor qs:QSTRING
  << spef_header_.vendor = text($qs); >>
  ;

program_name:
  TK_program qs:QSTRING
  << spef_header_.program = text($qs); >>
  ;

program_version:
  TK_version qs:QSTRING
  << spef_header_.program_version = text($qs); >>
  ;

design_flow:
  TK_design_flow (qs:QSTRING << parse_design_flow(text($qs)); >>)+
  ;

hierarchy_div_def:
  TK_divider
  d:HCHAR_
  << hier_delim_ = text($d)[0]; >>
  ;

hier_delim:
//  << text(LT(1))[0] == hier_delim_ >>?
  HCHAR_
  ;

path > [std::string p]:
  pa:PATH_ << $p = text($pa); >>
  ;

pb_delim:
//  << text(LT(1))[0] == pb_delim_ >>?
  PB_DELIM_
  ;

sb_delim:
//  << text(LT(1))[0] == sb_delim_ >>?
  SB_DELIM_
  ;

bit_identifier > [std::string b]:
  bi:BIT_IDENTIFIER_ << $b = text($bi); >>
  ;

pin_delim_def:
  TK_delimiter
  d:HCHAR_
  << pin_delim_ = text($d)[0]; >>
  ;

pin_delim:
//  << text(LT(1))[0] == pin_delim_ >>?
  HCHAR_
  ;

bus_delim_def:
  TK_bus_delimiter
  p:PB_DELIM_
  << pb_delim_ = text($p)[0];
     if (pb_delim_ == hier_delim_ || pb_delim_ == pin_delim_)
       db::err2($p, "Prefix bus delimiter should differ from"
                 " pin and hierarchy delimiters");
  >>
  { s:SB_DELIM_
    << bool matches = false;
       sb_delim_ = text($s)[0];
       switch (pb_delim_)
       {
       case '[': matches = sb_delim_ == ']'; break;
       case '{': matches = sb_delim_ == '}'; break;
       case '(': matches = sb_delim_ == ')'; break;
       case '<': matches = sb_delim_ == '>'; break;
       }
       if (! matches)
         db::warn2($s, "`%c' doesn't match `%c'", sb_delim_, pb_delim_);
    >>
  }
  ;

unit_def: time_scale cap_scale res_scale induc_scale
  ;

time_scale:
  TK_t_unit pos_number>[time_scale_] tu:TIME_UNIT // ( NS | PS )
  << if (text($tu)[0] == 'P')
       time_scale_ /= 1000;        // Scale to NS.
  >>
  ;

cap_scale:
  TK_c_unit pos_number>[cap_scale_] cu:CAP_UNIT // ( PF | FF )
  << if (text($cu)[0] == 'F')
       cap_scale_ /= 1000;         // Scale to PF.
  >>
  ;

res_scale:
  TK_r_unit pos_number>[res_scale_] ru:RES_UNIT // ( OHM | KOHM )
  << if (text($ru)[0] == 'O')
       res_scale_ /= 1000;         // Scale to KOHM.
  >>
  ;

induc_scale:
  << char c; >>
  TK_l_unit pos_number>[induc_scale_] iu:INDUC_UNIT // ( HENRY | MH | UH )
  << c = text($iu)[0];
     if (c == 'H')
       induc_scale_ *= 1000000;    // Scale to UH.
     else if (c == 'M')
       induc_scale_ *= 1000;       // Scale to UH.
  >>
  ;

// 9.3.2.3 Name map definition
name_map: TK_name_map name_map_entry (name_map_entry)*
  ;

name_map_entry:
        << int ind; std::string s; >>
        index>[ind] mapped_item>[s]
        << strip_path(&s);
           std::pair<NameMapConn::iterator, bool> p =
             NameMap_.insert(std::make_pair(ind, s));
           if (! p.second)
             db::warn("Duplicate index `*%d' in NAME_MAP, ignored", ind);
        >>
  ;

index > [int ind]:
       i:INDEX
       << $ind = atoi(text($i) + 1); >> // Adds one to skip over '*'.
  ;

mapped_item > [std::string t]:
    qstring>[$t]
  | path>[$t] // ( IDENT | BIT_IDENT | PATH | NAME | PHYSICAL_REF )
  ;

physical_ref > [std::string r]:
  pr:PHYSICAL_REF_ << $r = text($pr); >>
  ;

name > [std::string nm]:
     n:NAME_ << $nm = text($n); >>
  ;

pos_integer > [int i]:
    n:POS_INTEGER << $i = atoi(text($n)); >>
  ;

pos_number > [double d]:
    n:POS_NUMBER_ << $d = atof(text($n)); >>
  ;

pos_fraction > [double d]:
    n:POS_FRACTION << $d = atof(text($n)); >>
  ;

float_ > [double d]:
    n:FLOAT_ << $d = atof(text($n)); >>
  ;

number > [double d]:
    n:NUMBER_ << $d = atof(text($n)); >>
  ;

ident > [std::string id]:
    i:IDENT_ << $id = text($i); >>
  ;

qstring > [std::string s]:
    qs:QSTRING << $s = text($qs); >>
  ;

// 9.3.2.4 Power and ground nets definition
power_def: power_net_def {ground_net_def} | ground_net_def
  ;

power_net_def: TK_power_nets net_name (net_name)*
  ;

net_name:
        << std::string ref; >>
        ( net_ref>[ref] )? | pnet_ref>[ref]
  ;

net_ref > [std::string p]:
           << int ind; >>
           index>[ind]
           << $p = index_to_name(ind); >>
         | path>[$p]
  ;

pnet_ref > [std::string r]:
           << int ind; >>
            index>[ind]
           << $r = index_to_name(ind); >>
          | physical_ref>[$r]
  ;

ground_net_def: TK_ground_nets net_name (net_name)*
  ;

// 9.3.2.5 External definition

// Syntax 9-8 - Syntax for external definition

external_def: port_def {physical_port_def} | physical_port_def
  ;

port_def:
        TK_ports
        (<< t_spef_port *port; >>
         << port = new t_spef_port(thr_r_[0], thr_r_[1], thr_r_[2],
                                   thr_f_[0], thr_f_[1], thr_f_[2]); >>
         port_entry[port]
         << ports_[std::make_pair(port->inst, port->pin)] = port; >>
        )+
  ;

port_entry < [t_spef_port *port]:
        << std::string inst, pin; >>
        port_name[&inst, &pin]
        << port->type = t_spef_port::PORT_EXT;
           if (! find_port(inst, pin, &port->inst, &port->pin))
             db::warn("Component `%s%c%s' was not found",
                  inst.c_str(), pin_delim_, pin.c_str());
        >>
        direction[&port->direction]
        (conn_attr[&port->cap_load,
                   &port->slew_rise, &port->slew_fall,
                   &port->threshold_rise, &port->threshold_fall])*
  ;

port_name < [std::string *inst, std::string *pin]:
  pn:PORT_NAME_
  << name_info(text($pn), inst, pin); >>
  ;

inst_name > [std::string p]:
           << int ind; >>
           index>[ind]
           << $p = index_to_name(ind); >>
         | path>[$p]
  ;

port > [std::string b]:
           << int ind; >>
           index>[ind]
           << $b = index_to_name(ind); >>
         | bit_identifier>[$b]
  ;

direction < [char *d]:  // That is, pointer to a char, not C string.
           dir:DIRECTION // I | B | O
           << *d = text($dir)[0]; >>
  ;

physical_port_def:
        TK_physical_ports
        (<< t_spef_port *port; >>
         << port = new t_spef_port(thr_r_[0], thr_r_[1], thr_r_[2],
                                   thr_f_[0], thr_f_[1], thr_f_[2]); >>
         pport_entry[port]
         << ports_[std::make_pair(port->inst, port->pin)] = port; >>
        )+
  ;

pport_entry < [t_spef_port *port]:
        << std::string inst, pin; >>
        pport_name[&inst, &pin]
        << port->type = t_spef_port::PORT_EXT_PHYS;
           if (! find_port(inst, pin, &port->inst, &port->pin))
             db::warn("Component `%s%c%s'  was not found",
                  inst.c_str(), pin_delim_, pin.c_str());
        >>
        direction[&port->direction]
        (conn_attr[&port->cap_load,
                   &port->slew_rise, &port->slew_fall,
                   &port->threshold_rise, &port->threshold_fall])*
  ;

pport_name < [std::string *inst, std::string *pin]:
     pn:PPORT_NAME_
  << name_info(text($pn), inst, pin); >>
  ;

physical_inst > [std::string r]:
           << int ind; >>
           index>[ind]
           << $r = index_to_name(ind); >>
         | physical_ref>[$r]
  ;

pport > [std::string n]:
           << int ind; >>
           index>[ind]
           << $n = index_to_name(ind); >>
         | name>[$n]
  ;

// Syntax 9-9 - Syntax for connection attributes definition

conn_attr < [t_spef_triplet *capload,
             t_spef_triplet *slew_rise,
             t_spef_triplet *slew_fall,
             t_spef_triplet *threshold_rise,
             t_spef_triplet *threshold_fall]:
         coordinates
       | cap_load[capload]
       | slews[slew_rise, slew_fall, threshold_rise, threshold_fall]
       | driving_cell
  ;

coordinates: TK_c NUMBER_ NUMBER_
  ;

cap_load < [t_spef_triplet *pv]:
          TK_l par_value[pv, cap_scale_]
  ;

par_value < [t_spef_triplet *pv, double scale]:
    << double d0, d1, d2; >>
    t:PV_TRIPLET_
    << split_triplet(text($t), &d0, &d1, &d2);
       pv->value[0] = d0 * scale;
       pv->value[1] = d1 * scale;
       pv->value[2] = d2 * scale;
    >>
  ;

slews < [t_spef_triplet *rise,
         t_spef_triplet *fall,
         t_spef_triplet *threshold_rise,
         t_spef_triplet *threshold_fall]:
      TK_s
      par_value[rise, time_scale_] par_value[fall, time_scale_]
      {threshold[threshold_rise] threshold[threshold_fall]}
  ;

threshold < [t_spef_triplet *th]:
    << double d0, d1, d2; >>
    t:PF_TRIPLET_
    << split_triplet(text($t), &d0, &d1, &d2);
       th->value[0] = d0;
       th->value[1] = d1;
       th->value[2] = d2;
    >>
  ;

driving_cell: TK_d cell_type
  ;

cell_type: INDEX | NAME_
  ;

// 9.3.2.6 Hierarchical SPEF (entities) definition

// Syntax 9-10 - Syntax for entities definition

define_def: define_entry (define_entry)*
  ;

define_entry:
         << std::string name; >>
         TK_define (inst_name>[name])+ entity
       | TK_pdefine physical_inst>[name] entity
  ;

entity: QSTRING
  ;

// 9.3.2.7 Internal definition

// Syntax 9-11 - Syntax for internal definition

internal_def: (nets)+
  ;

nets:
      << t_spef_d_net *dnet; t_spef_r_net *rnet; >>
      << dnet = new t_spef_d_net(conf_);
         dnet->type = t_spef_net::D_NET; >>
      d_net[dnet]
      << spef_nets_.push_back(dnet); >>
    | << rnet = new t_spef_r_net(conf_);
         rnet->type = t_spef_net::R_NET; >>
      r_net[rnet]
      << spef_nets_.push_back(rnet); >>
    | << dnet = new t_spef_d_net(conf_);
         dnet->type = t_spef_net::D_PNET; >>
      d_pnet[dnet]
      << spef_nets_.push_back(dnet); >>
    | << rnet = new t_spef_r_net(conf_);
         rnet->type = t_spef_net::R_PNET; >>
      r_pnet[rnet]
      << spef_nets_.push_back(rnet); >>
  ;

// 9.3.2.7.1 d_net definition

// Syntax 9-12 - Syntax for d_net definition

d_net < [t_spef_d_net *net]:
       << std::string netref; >>
       TK_d_net
       net_ref>[netref]
       << if (! find_net(netref, &net->net_index))
            db::warn("Net `%s' was not found", netref.c_str());
          current_net_ = net;
       >>
       total_cap[&net->total_cap]
       {routing_conf[&net->conf]}
       {conn_sec[&net->pins, &net->n_points]}
       {cap_sec[&net->cap]}
       {res_sec[&net->res]}
       {induc_sec[&net->ind]}
       TK_end
  ;

total_cap < [t_spef_triplet *c]:
      par_value[c, cap_scale_]
  ;

routing_conf < [char *c]:
      TK_v conf[c]
  ;

conf < [char *c]:
     pos_integer>[*c]
  ;

// Syntax 9-13 - Syntax for d_net connectivity section definition

conn_sec < [t_spef_d_net::PinConn *pins, int *n_points]:
        TK_conn
        (<< t_spef_port pin(thr_r_[0], thr_r_[2], thr_r_[3],
                            thr_f_[0], thr_f_[1], thr_f_[2]); >>
         << pin = t_spef_port(thr_r_[0], thr_r_[1], thr_r_[2],
                              thr_f_[0], thr_f_[1], thr_f_[2]); >>
         conn_def[&pin]
         << pins->insert(std::make_pair(std::make_pair(pin.inst, pin.pin),
                                        pin));
         >>
        )+
        (internal_node_coord << *n_points++; >>)*
  ;

conn_def < [t_spef_port *port]:
          << std::string sinst, spin; >>
          TK_p
          external_connection[&sinst, &spin]
          << int inst, pin;
             if (! find_port(sinst, spin, &inst, &pin))
               db::warn("Component `%s%c%s' was not found",
                    sinst.c_str(), pin_delim_, spin.c_str());
  
             PortConn::iterator pi;
             pi = ports_.find(std::make_pair(inst, pin));
             if (pi != ports_.end())
               *port = *pi->second;
             else {
               db::warn("Port or pport `%s%c%s' should be defined in *PORTS"
                    " or *PHYSICAL_PORTS section first",
                    sinst.c_str(), pin_delim_, spin.c_str());
               port->type = t_spef_port::PORT_EXT; // FIXME: _PHYS ?
               port->inst = inst;
               port->pin = pin;
             }
          >>
          direction[&port->direction]
          (conn_attr[&port->cap_load,
                     &port->slew_rise, &port->slew_fall,
                     &port->threshold_rise, &port->threshold_fall])*
        | TK_i
          internal_connection[&sinst, &spin]
          << int inst, pin;
             if (! find_port(sinst, spin, &inst, &pin))
               db::warn("Component `%s%c%s' was not found",
                    sinst.c_str(), pin_delim_, spin.c_str());
  
             port->type = t_spef_port::PIN_INT; // FIXME: _PHYS ?
             port->inst = inst;
             port->pin = pin;
          >>
          direction[&port->direction]
          (conn_attr[&port->cap_load,
                     &port->slew_rise, &port->slew_fall,
                     &port->threshold_rise, &port->threshold_fall])*
  ;

external_connection < [std::string *inst, std::string *pin]:
    ( port_name[inst, pin] )?
  | pport_name[inst, pin]
  ;

internal_connection < [std::string *inst, std::string *pin]:
    ( pin_name[inst, pin] )?
  | pnode_ref[inst, pin]
  ;

pin_name < [std::string *inst, std::string *pin]:
  pn:PIN_NAME_
  << name_info(text($pn), inst, pin); >>
  ;

pin > [std::string b]:
           << int ind; >>
           index>[ind]
           << $b = index_to_name(ind); >>
         | bit_identifier>[$b]
  ;

pnode_ref < [std::string *inst, std::string *pin]:
  pn:PNODE_REF_
  << name_info(text($pn), inst, pin); >>
  ;

pnode > [std::string n]:
       << int ind; >>
       index>[ind]
       << $n = index_to_name(ind); >>
     | name>[$n]
  ;

internal_node_coord:
  << std::string inst, pin; >>
  TK_n internal_node_name[&inst, &pin] coordinates
  ;

internal_node_name < [std::string *net, std::string *node]:
        in:INT_NODE_NAME_
        << name_info(text($in), net, node); >>
  ;

// Syntax 9-14 - Syntax for d_net capacitance section definition

cap_sec < [std::vector<t_spef_edge> *caps]:
        TK_cap
        (<< t_spef_edge c; >>
         << c = t_spef_edge(); >>
         cap_elem[&c]
         << caps->push_back(c); >>
        )+
  ;

cap_elem < [t_spef_edge *c]:
       cap_id node_name[&c->node1]
       ( ( node_name2[&c->node2] par_value[&c->value, cap_scale_] )?
       | par_value[&c->value, cap_scale_]
         << c->node2.type = t_spef_node::NODE_GROUND; >>
       )
  ;

cap_id: POS_INTEGER
  ;

node_name < [t_spef_node *node]:
  << std::string sinst, spin; >>
  (   ( internal_node_name[&sinst, &spin] )?
    | ( internal_connection[&sinst, &spin] )?
    | ( external_connection[&sinst, &spin] )?
    | pnode_ref[&sinst, &spin]
  )
  << int inst, pin;
     if (find_int_node(sinst, spin, &inst, &pin)) {
       node->type = t_spef_node::NODE_NET_POINT;
       node->index1 = pin;
     } else if (find_int_conn(sinst, spin, &inst, &pin)) {
       node->type = t_spef_node::NODE_INT_PIN;
       node->index1 = inst;
       node->index2 = pin;
     } else if (find_port(sinst, spin, &inst, &pin)) {
       node->type = t_spef_node::NODE_EXT_PORT;
       node->index1 = inst;
     } else {
       db::warn("Component `%s%c%s' was not found",
            sinst.c_str(), pin_delim_, spin.c_str());
     }
  >>
  ;

node_name2 < [t_spef_node *node]:
  << std::string sinst, spin; >>
  (   ( pnn:INT_PNODE_NAME_ )?
      << name_info(text($pnn), &sinst, &spin); >>
    | nn:INT_NODE_NAME_
      << name_info(text($nn), &sinst, &spin); >>
  )?
  << int inst, pin;
     if (find_int_node(sinst, spin, &inst, &pin)) {
       node->type = t_spef_node::NODE_NET_POINT;
       node->index1 = pin;
     } else if (find_int_conn(sinst, spin, &inst, &pin)) {
       node->type = t_spef_node::NODE_INT_PIN;
       node->index1 = inst;
       node->index2 = pin;
     } else {
       db::warn("Component `%s%c%s' was not found",
            sinst.c_str(), pin_delim_, spin.c_str());
     }
  >>
  | node_name[node]
  ;

net_ref2 > [std::string inst]:
         net_ref>[$inst]
  ;

// Syntax 9-15 - Syntax for d_net resistance section definition

res_sec < [std::vector<t_spef_edge> *ress]:
        TK_res
        (<< t_spef_edge r; >>
         << r = t_spef_edge(); >>
         res_elem[&r]
         << ress->push_back(r); >>
        )+
  ;

res_elem < [t_spef_edge *r]:
       res_id
       node_name[&r->node1] node_name[&r->node2]
       par_value[&r->value, res_scale_]
  ;

res_id: POS_INTEGER
  ;

// Syntax 9-16 - Syntax for d_net inductance section definition

induc_sec < [std::vector<t_spef_edge> *inds]:
        TK_induc
        (<< t_spef_edge i; >>
         << i = t_spef_edge(); >>
         induc_elem[&i]
         << inds->push_back(i); >>
        )+
  ;

induc_elem < [t_spef_edge *i]:
       induc_id
       node_name[&i->node1] node_name[&i->node2]
       par_value[&i->value, induc_scale_]
  ;

induc_id: POS_INTEGER
  ;

// 9.3.2.7.2 r_net definition

// Syntax 9-17 - Syntax for r_net definition

r_net < [t_spef_r_net *net]:
       << std::string netref; >>
       TK_r_net
       net_ref>[netref]
       << if (! find_net(netref, &net->net_index))
            db::warn("Net `%s' was not found", netref.c_str());
          current_net_ = net;
       >>
       total_cap[&net->total_cap]
       {routing_conf[&net->conf]}
       (<< t_spef_driver_reduc dr; >>
        << dr = t_spef_driver_reduc(); >>
        driver_reduc[&dr]
        << net->driver_reduc.push_back(dr); >>
       )*
       TK_end
  ;

driver_reduc < [t_spef_driver_reduc *dr]:
       driver_pair[&dr->inst, &dr->pin]
       driver_cell
       pie_model[&dr->c2, &dr->r1, &dr->c1]
       load_desc[&dr->rc_desc]
  ;

driver_pair < [int *inst, int *pin]:
  << std::string sinst, spin; >>
  TK_driver pin_name[&sinst, &spin]
  << if (! find_pin_name(sinst, spin, inst, pin))
       db::warn("Component `%s%c%s' was not found",
            sinst.c_str(), pin_delim_, spin.c_str());
  >>
  ;

driver_cell: TK_cell cell_type
  ;

pie_model < [t_spef_triplet *c2, t_spef_triplet *r1,
             t_spef_triplet *c1]:
       TK_c2_r1_c1
       par_value[c2, cap_scale_]
       par_value[r1, res_scale_]
       par_value[c1, cap_scale_]
  ;

// Syntax 9-18 - Syntax for r_net load descriptions definition

load_desc < [std::vector<t_spef_rc_desc> *rc]:
         TK_loads
         (<< t_spef_rc_desc d; >>
          << d = t_spef_rc_desc(); >>
          rc_desc[&d]
          << rc->push_back(d); >>
         )+
  ;

rc_desc < [t_spef_rc_desc *d]:
  << std::string sinst, spin; >>
  TK_rc
  pin_name[&sinst, &spin]
  << if (! find_pin_name(sinst, spin, &d->inst, &d->pin))
       db::warn("Component `%s%c%s' was not found",
            sinst.c_str(), pin_delim_, spin.c_str());
  >>
  par_value[&d->rc, time_scale_]
  {pole_residue_desc[&d->pole, &d->residue]}
  ;

pole_residue_desc < [std::vector<t_spef_ctriplet> *pole,
                     std::vector<t_spef_ctriplet> *residue]:
         pole_desc[pole] residue_desc[residue]
  ;

pole_desc < [std::vector<t_spef_ctriplet> *pol]:
      TK_q
      POS_INTEGER
      (<< t_spef_ctriplet p; >>
       << p = t_spef_ctriplet(); >>
       pole[&p]
       << pol->push_back(p); >>
      )+
  ;

pole < [t_spef_ctriplet *p]:
     complex_par_value[p]
  ;

complex_par_value < [t_spef_ctriplet *cpv]:
    << double d0, d1, d2; >>
    ( pv:PV_TRIPLET_ )?
    << split_triplet(text($pv), &d0, &d1, &d2);
       cpv->value[0].real = d0;
       cpv->value[1].real = d1;
       cpv->value[2].real = d2;
       cpv->value[0].img = cpv->value[1].img = cpv->value[2].img = 0;
    >>
  | number>[cpv->value[0].real]
    << cpv->value[0].img = cpv->value[1].img =
       cpv->value[2].img = 0; >>
    ( COLON number>[cpv->value[1].real]
      COLON number>[cpv->value[2].real]
    | << cpv->value[2].real = cpv->value[1].real =
         cpv->value[0].real; >>
    )
  | cnumber[&cpv->value[0]]
    ( COLON cnumber[&cpv->value[1]]
      COLON cnumber[&cpv->value[2]]
    | << cpv->value[2] = cpv->value[1] = cpv->value[0]; >>
    )
  ;

cnumber < [t_cfloat *cf]:
         LPAREN
         number>[cf->real]
         number>[cf->img]
         RPAREN
  ;

residue_desc < [std::vector<t_spef_ctriplet> *resid]:
        TK_k
        POS_INTEGER
        (<< t_spef_ctriplet r; >>
         << r = t_spef_ctriplet(); >>
         residue[&r]
         << resid->push_back(r); >>
        )+
  ;

residue < [t_spef_ctriplet *r]:
        complex_par_value[r]
  ;

// 9.3.2.7.3 d_pnet definition

// Syntax 9-19 - Syntax for d_pnet definition

d_pnet < [t_spef_d_net *net]:
       << std::string netref; >>
       TK_d_pnet
       pnet_ref>[netref]
       << if (! find_net(netref, &net->net_index))
            db::warn("Net `%s' was not found", netref.c_str());
          current_net_ = net;
       >>
       total_cap[&net->total_cap]
       {routing_conf[&net->conf]}
       {pconn_sec[&net->pins, &net->n_points]}
       {pcap_sec[&net->cap]}
       {pres_sec[&net->res]}
       {pinduc_sec[&net->ind]}
       TK_end
  ;

// Syntax 9-20 - Syntax for d_pnet connectivity section definition

pconn_sec < [t_spef_d_net::PinConn *pins, int *n_points]:
        TK_conn
        (<< t_spef_port pin(thr_r_[0], thr_r_[2], thr_r_[3],
                            thr_f_[0], thr_f_[1], thr_f_[2]); >>
         << pin = t_spef_port(thr_r_[0], thr_r_[1], thr_r_[2],
                              thr_f_[0], thr_f_[1], thr_f_[2]); >>
         pconn_def[&pin]
         << pins->insert(std::make_pair(std::make_pair(pin.inst, pin.pin),
                                        pin));
         >>
        )+
        << *n_points = 0; >>
        (internal_pnode_coord << *n_points++; >>)*
  ;

pconn_def < [t_spef_port *port]:
          << std::string sinst, spin; >>
          TK_p
          pexternal_connection[&sinst, &spin]
          << int inst, pin;
             if (! find_port(sinst, spin, &inst, &pin))
               db::warn("Component `%s%c%s' was not found",
                    sinst.c_str(), pin_delim_, spin.c_str());
  
             PortConn::iterator pi;
             pi = ports_.find(std::make_pair(inst, pin));
             if (pi != ports_.end())
               *port = *pi->second;
               if (pi->second->type != t_spef_port::PORT_EXT_PHYS)
                 db::warn("`%s%c%s' is not a pport",
                      sinst.c_str(), pin_delim_, spin.c_str());
             else {
               db::warn("Pport `%s%c%s' should be defined in"
                    " *PHYSICAL_PORTS section first",
                    sinst.c_str(), pin_delim_, spin.c_str());
               port->type = t_spef_port::PORT_EXT_PHYS;
               port->inst = inst;
               port->pin = pin;
             }
          >>
          direction[&port->direction]
          (conn_attr[&port->cap_load,
                     &port->slew_rise, &port->slew_fall,
                     &port->threshold_rise, &port->threshold_fall])*
        | TK_i
          internal_connection[&sinst, &spin]
          << int inst, pin;
             if (! find_port(sinst, spin, &inst, &pin))
               db::warn("Component `%s%c%s' was not found",
                    sinst.c_str(), pin_delim_, spin.c_str());
  
             port->type = t_spef_port::PIN_INT; // FIXME: _PHYS ?
             port->inst = inst;
             port->pin = pin;
          >>
          direction[&port->direction]
          (conn_attr[&port->cap_load,
                     &port->slew_rise, &port->slew_fall,
                     &port->threshold_rise, &port->threshold_fall])*
  ;

pexternal_connection < [std::string *inst, std::string *pin]:
  pport_name[inst, pin]
  ;

internal_pnode_coord:
  << std::string inst, pin; >>
  TK_n internal_pnode_name[&inst, &pin] coordinates
  ;

internal_pnode_name < [std::string *net, std::string *node]:
  in:INT_PNODE_REF_
  << name_info(text($in), net, node); >>
  ;

// Syntax 9-21 - Syntax for d_pnet capacitance section definition

pcap_sec < [std::vector<t_spef_edge> *caps]:
         TK_cap
        (<< t_spef_edge c; >>
         << c = t_spef_edge(); >>
         pcap_elem[&c]
         << caps->push_back(c); >>
        )+
  ;

pcap_elem < [t_spef_edge *c]:
       cap_id pnode_name[&c->node1]
       { ( pnode_name2[&c->node2] par_value[&c->value, cap_scale_] )?
         pnode_name2[&c->node2]
       }
       par_value[&c->value, cap_scale_]
  ;

pnode_name < [t_spef_node *node]:
  << std::string sinst, spin; >>
  (   ( internal_pnode_name[&sinst, &spin] )?
    | ( internal_connection[&sinst, &spin] )?
    | ( pexternal_connection[&sinst, &spin] )?
    | pnode_ref[&sinst, &spin]
  )
  << int inst, pin;
     if (find_int_node(sinst, spin, &inst, &pin)) {
       node->type = t_spef_node::NODE_NET_POINT;
       node->index1 = pin;
     } else if (find_int_conn(sinst, spin, &inst, &pin)) {
       node->type = t_spef_node::NODE_INT_PIN;
       node->index1 = inst;
       node->index2 = pin;
     } else if (find_port(sinst, spin, &inst, &pin)) {
       node->type = t_spef_node::NODE_EXT_PORT;
       node->index1 = inst;
     } else {
       db::warn("Component `%s%c%s' was not found",
            sinst.c_str(), pin_delim_, spin.c_str());
     }
  >>
  ;

pnode_name2 < [t_spef_node *node]:
  << std::string sinst, spin; >>
  (   ( pnn:INT_PNODE_NAME_ )?
      << name_info(text($pnn), &sinst, &spin); >>
    | nn:INT_NODE_NAME_
      << name_info(text($nn), &sinst, &spin); >>
  )?
  << int inst, pin;
     if (find_int_node(sinst, spin, &inst, &pin)) {
       node->type = t_spef_node::NODE_NET_POINT;
       node->index1 = pin;
     } else if (find_int_conn(sinst, spin, &inst, &pin)) {
       node->type = t_spef_node::NODE_INT_PIN;
       node->index1 = inst;
       node->index2 = pin;
     } else {
       db::warn("Component `%s%c%s' was not found",
            sinst.c_str(), pin_delim_, spin.c_str());
     }
  >>
  | pnode_name[node]
  ;

pnet_ref2 > [std::string inst]:
         pnet_ref>[$inst]
  ;

// Syntax 9-22 - Syntax for d_pnet resistance section definition

pres_sec < [std::vector<t_spef_edge> *ress]:
        TK_res
        (<< t_spef_edge r; >>
         << r = t_spef_edge(); >>
         pres_elem[&r]
         << ress->push_back(r); >>
        )+
  ;

pres_elem < [t_spef_edge *r]:
       res_id
       pnode_name[&r->node1] pnode_name[&r->node2]
       par_value[&r->value, res_scale_]
  ;

// Syntax 9-23 - Syntax for d_pnet inductance section definition

pinduc_sec < [std::vector<t_spef_edge> *inds]:
        TK_induc
        (<< t_spef_edge i; >>
         << i = t_spef_edge(); >>
         pinduc_elem[&i]
         << inds->push_back(i); >>
        )+
  ;

pinduc_elem < [t_spef_edge *i]:
      induc_id
      pnode_name[&i->node1] pnode_name[&i->node2]
      par_value[&i->value, induc_scale_]
  ;

// 9.3.2.7.4 r_pnet definition

// Syntax 9-14 - Syntax for r_pnet definition

r_pnet < [t_spef_r_net *net]:
       << std::string netref; >>
       TK_r_pnet
       pnet_ref>[netref]
       << if (! find_net(netref, &net->net_index))
            db::warn("Net `%s' was not found", netref.c_str());
          current_net_ = net;
       >>
       total_cap[&net->total_cap]
       {routing_conf[&net->conf]}
       (<< t_spef_driver_reduc dr; >>
        << dr = t_spef_driver_reduc(); >>
        pdriver_reduc[&dr]
        << net->driver_reduc.push_back(dr); >>
       )*
       TK_end
  ;

pdriver_reduc < [t_spef_driver_reduc *dr]:
       pdriver_pair[&dr->inst, &dr->pin]
       driver_cell
       pie_model[&dr->c2, &dr->r1, &dr->c1]
       load_desc[&dr->rc_desc]
  ;

pdriver_pair < [int *inst, int *pin]:
  << std::string sinst, spin; >>
  TK_driver internal_connection[&sinst, &spin]
  << if (! find_int_conn(sinst, spin, inst, pin))
       db::warn("Component `%s%c%s' was not found",
            sinst.c_str(), pin_delim_, spin.c_str());
  >>
  ;
