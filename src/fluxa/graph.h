// copyright (C) 2006 david griffiths <dave@pawfal.org>
//
// this program is free software; you can redistribute it and/or modify
// it under the terms of the GNU general public license as published by
// the free software foundation; either version 2 of the license, or
// (at your option) any later version.
//
// this program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  see the
// GNU general public license for more details.
//
// you should have received a copy of the GNU general public license
// along with this program; if not, write to the free software
// foundation, inc., 59 temple place - suite 330, boston, MA 02111-1307, USA.

#include <vector>
#include <map>
#include <set>
#include <list>
#include <math.h>
#include "graph_node.h"
#include "module_nodes.h"
#include "event_queue.h"

#ifndef GRAPH
#define GRAPH

class graph {
 public:
  graph(unsigned int num_nodes, unsigned int sample_rate);
  ~graph();

  enum node_type{TERMINAL,SINOSC,SAWOSC,TRIOSC,SQUOSC,WHITEOSC,PINKOSC,ADSR,ADD,SUB,MUL,DIV,POW,
		 MOOGLP,MOOGBP,MOOGHP,FORMANT,SAMPLER,CRUSH,DISTORT,CLIP,DELAY,KS,XFADE,SAMPNHOLD,
		 TRACKNHOLD,PAD,NUMTYPES};

  void init();
  void clear();
  void create(unsigned int id, node_type t, float v);
  void connect(unsigned int id, unsigned int arg, unsigned int to);
  void play(unsigned int seconds, unsigned int fraction, unsigned int id, float pan);
  void process(unsigned int bufsize, sample &left, sample &right);

 private:
  void _play(float time, unsigned int id, float pan);

  class node_desc {
  public:
  node_desc(): m_node(NULL), m_ID(0) {}
    graph_node *m_node;
    unsigned int m_ID;
  };

  class node_desc_vec {
  public:
    unsigned int new_index() {
      m_current++;
      if (m_current>=m_vec.size()) {
	//cerr<<"going round..."<<endl;
	m_current=0;
      }
      return m_current;
    }

    node_desc_vec(): m_current(0) {}
    unsigned int m_current;
    vector<node_desc*> m_vec;
  };

  std::list<pair<unsigned int, float> > m_root_nodes;
  map<unsigned int,graph_node*> m_node_map;
  map<node_type,node_desc_vec*> m_node_desc_map;
  unsigned int m_num_nodes;
  unsigned int m_sample_rate;

  event_queue m_event_queue;
  spiralcore::time m_current_time;
};

#endif
