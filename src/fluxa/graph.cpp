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

#include <iostream>
using namespace std;

#include <vector>
#include <math.h>
#include "graph.h"
#include "module_nodes.h"
#include "modules.h"

graph::graph(unsigned int num_nodes, unsigned int sample_rate) :
  m_num_nodes(num_nodes),
  m_sample_rate(sample_rate) 
{
  wave_table::write_waves();
  init();
  m_mutex = new pthread_mutex_t;
  pthread_mutex_init(m_mutex,NULL);
  m_current_time.set_to_now();
}

graph::~graph() {
  clear();
}

void graph::init() {
  for (unsigned int type=0; type<NUMTYPES; type++) {
    node_desc_vec *descvec = new node_desc_vec;

    unsigned int count=m_num_nodes;

    if (type==TERMINAL) count=200;
    if (type==SAMPLER) count=0;

    for(unsigned int n=0; n<count; n++) {
      node_desc *nodedesc = new node_desc;
	
      switch(type) {
      case TERMINAL : nodedesc->m_node = new terminal_node(0); break;
      case SINOSC : nodedesc->m_node = new osc_node((int)wave_table::SINE,m_sample_rate); break;
      case SAWOSC : nodedesc->m_node = new osc_node((int)wave_table::SAW,m_sample_rate); break;
      case TRIOSC : nodedesc->m_node = new osc_node((int)wave_table::TRIANGLE,m_sample_rate); break;
      case SQUOSC : nodedesc->m_node = new osc_node((int)wave_table::SQUARE,m_sample_rate); break;
      case WHITEOSC : nodedesc->m_node = new osc_node((int)wave_table::NOISE,m_sample_rate); break;
      case PINKOSC : nodedesc->m_node = new osc_node((int)wave_table::PINKNOISE,m_sample_rate); break;
      case ADSR : nodedesc->m_node = new ADSRnode(m_sample_rate); break;
      case ADD : nodedesc->m_node = new math_node(math_node::ADD); break;
      case SUB : nodedesc->m_node = new math_node(math_node::SUB); break;
      case MUL : nodedesc->m_node = new math_node(math_node::MUL); break;
      case DIV : nodedesc->m_node = new math_node(math_node::DIV); break;
      case POW : nodedesc->m_node = new math_node(math_node::POW); break;
      case MOOGLP : nodedesc->m_node = new filter_node(filter_node::MOOGLP,m_sample_rate); break;
      case MOOGBP : nodedesc->m_node = new filter_node(filter_node::MOOGBP,m_sample_rate); break;
      case MOOGHP : nodedesc->m_node = new filter_node(filter_node::MOOGHP,m_sample_rate); break;
      case FORMANT : nodedesc->m_node = new filter_node(filter_node::FORMANT,m_sample_rate); break;
	//		case SAMPLER : nodedesc->m_node = new sample_node(m_sample_rate); break;
      case CRUSH : nodedesc->m_node = new effect_node(effect_node::CRUSH,m_sample_rate); break;
      case DISTORT : nodedesc->m_node = new effect_node(effect_node::DISTORT,m_sample_rate); break;
      case CLIP : nodedesc->m_node = new effect_node(effect_node::CLIP,m_sample_rate); break;
      case DELAY : nodedesc->m_node = new effect_node(effect_node::DELAY,m_sample_rate); break;
      case KS : nodedesc->m_node = new KSnode(m_sample_rate); break;
      case XFADE : nodedesc->m_node = new Xfade_node(); break;
      case SAMPNHOLD : nodedesc->m_node = new hold_node(hold_node::SAMP); break;
      case TRACKNHOLD : nodedesc->m_node = new hold_node(hold_node::TRACK); break;
      case PAD : nodedesc->m_node = new pad_node(m_sample_rate); break;
      default: assert(0); break;
      }

      descvec->m_vec.push_back(nodedesc);
    }

    m_node_desc_map[(node_type)type] = descvec;
  }
}

void graph::clear() {
  m_root_nodes.clear();
  m_node_map.clear();

  for (map<node_type,node_desc_vec*>::iterator i=m_node_desc_map.begin();
       i!=m_node_desc_map.end(); ++i) {
    for (vector<node_desc*>::iterator ni=i->second->m_vec.begin();
	 ni!=i->second->m_vec.end(); ++ni) {
      delete (*ni)->m_node;
    }
    i->second->m_vec.clear();
  }

  m_node_desc_map.clear();
}

void graph::create(unsigned int id, node_type t, float v)
{
  pthread_mutex_lock(m_mutex);
  {
    unsigned int index=m_node_desc_map[t]->new_index();
    unsigned int oldid=m_node_desc_map[t]->m_vec[index]->m_ID;

    //cerr<<"create id:"<<id<<" index:"<<index<<" type:"<<t<<" value:"<<v<<endl;

    map<unsigned int,graph_node*>::iterator i=m_node_map.find(oldid);
    
    if (i!=m_node_map.end()) {
      m_node_map.erase(i);
      unsigned int root_id=i->second->get_root_id();

      // search for the old id in the root nodes
      list<pair<unsigned int,float> >::iterator remove;
      bool found=false;
      for (list<pair<unsigned int,float> >::iterator ri=m_root_nodes.begin();
	   ri!=m_root_nodes.end(); ++ri) {
	if (ri->first==root_id) {
	  remove=ri;
	  found=true;
	}
      }
      // remove it if found
      if (found) m_root_nodes.erase(remove);
    }

    m_node_desc_map[t]->m_vec[index]->m_ID=id;
    m_node_desc_map[t]->m_vec[index]->m_node->clear();
    m_node_map[id]=m_node_desc_map[t]->m_vec[index]->m_node;

    if (t==TERMINAL) {
      terminal_node *terminal = dynamic_cast<terminal_node*>(m_node_map[id]);
      assert(terminal!=NULL);
      terminal->set_value(v);
    }
    pthread_mutex_unlock(m_mutex);
  }
}

void graph::connect(unsigned int id, unsigned int arg, unsigned int to) {
  pthread_mutex_lock(m_mutex);
  {
    //cerr<<"connect id "<<id<<" arg "<<arg<<" to "<<to<<endl;
    if (m_node_map[id]!=NULL && m_node_map[to]!=NULL) {
      m_node_map[id]->set_child(arg,m_node_map[to]);
    }
    pthread_mutex_unlock(m_mutex);
  }
}

void graph::_play(float time, unsigned int id, float pan) {
  //    pthread_mutex_lock(m_mutex);
  {
    //cerr<<"play id "<<id<<endl;
    if (m_node_map[id]!=NULL) {
      m_node_map[id]->register_root(id);
      m_node_map[id]->trigger(time);
      m_root_nodes.push_back(pair<unsigned int, float>(id,pan));
    }
    // pthread_mutex_unlock(m_mutex);
  }
}

#define COUNTDOWN_TIMER 999
static int time_reset_countdown=0;

void graph::process(unsigned int bufsize, sample &left, sample &right) {
  pthread_mutex_lock(m_mutex);
  {
    spiralcore::time last_time = m_current_time;
    m_current_time.inc_by_sample(bufsize,m_sample_rate);

    if (time_reset_countdown--<0) {
      m_current_time.set_to_now();
      time_reset_countdown = COUNTDOWN_TIMER;
    }
    
    // first check the event queue
    event e;
    while (m_event_queue.get(last_time, m_current_time, e)) {
      
      float t = last_time.get_difference(e.time_stamp);
      // hack to get round bug with get_difference throwing big numbers
      if (t<=0) {
	_play(t,e.ID,e.pan);
      } else {
	cerr<<"----------------"<<endl;
	cerr<<t<<endl;
	last_time.print();
	e.time_stamp.print();
      }
    }

    for(list<pair<unsigned int, float> >::iterator i=m_root_nodes.begin();
	i!=m_root_nodes.end(); ++i) {
      if (m_node_map[i->first]!=NULL) {
	m_node_map[i->first]->process(bufsize);
	
	// do stereo panning
	float pan = i->second;
	float leftpan=1,rightpan=1;
	if (pan<0) leftpan=1-pan;
	else rightpan=1+pan;
	
	left.mul_mix(m_node_map[i->first]->get_output(),0,0.1*leftpan);
	right.mul_mix(m_node_map[i->first]->get_output(),0,0.1*rightpan);
      }
    }

    pthread_mutex_unlock(m_mutex);

  }
}


void graph::play(unsigned int seconds, unsigned int fraction, unsigned int id, float pan) {
  pthread_mutex_lock(m_mutex);
  event e;
  e.time_stamp.seconds=seconds;
  e.time_stamp.fraction=fraction;
  e.ID=id;
  e.pan=pan;

  // play-now (ish)
  if (e.time_stamp.seconds==0 && e.time_stamp.fraction==0) {
    e.time_stamp=m_current_time;
    e.time_stamp+=0.1;
  }
  if (e.time_stamp>=m_current_time) {
    m_event_queue.add(e);
    
    if (e.time_stamp.get_difference(m_current_time)>30) {
      cerr<<"reset clock? event far in future: "<<e.time_stamp.get_difference(m_current_time)<<endl;
    }
  } else {
    cerr<<"event arrived too late ignoring: "<<m_current_time.get_difference(e.time_stamp)<<endl;
    
    //m_current_time.print();
    //e.time_stamp=m_current_time;
    //e.time_stamp+=0.1;
    //m_event_queue.add(e);
  }
  pthread_mutex_unlock(m_mutex);

}
