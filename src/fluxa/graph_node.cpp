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

#include "graph_node.h"
#include <iostream>
#include <string>

using namespace std;

#define DEFAULT_BUFSIZE 4096*2

///////////////////////////////////////////

graph_node::graph_node(unsigned int numinputs) {
  for(unsigned int n=0; n<numinputs; n++) {
    m_child_nodes.push_back(NULL);
  }

  m_output.allocate(DEFAULT_BUFSIZE);
}

graph_node::~graph_node()
{
  clear();
}

void graph_node::register_root(unsigned int root_id)
{
  m_root_id = root_id;
  for(vector<graph_node*>::iterator i=m_child_nodes.begin();
      i!=m_child_nodes.end(); ++i) {
    if (*i!=NULL) {
      (*i)->register_root(root_id);
    }
  }
}

void graph_node::trigger_children(float time)
{
  for(vector<graph_node*>::iterator i=m_child_nodes.begin();
      i!=m_child_nodes.end(); ++i) {
    if (*i!=NULL) {
      (*i)->trigger(time);
    }
  }
}

void graph_node::process_children(unsigned int bufsize) {
  for(vector<graph_node*>::iterator i=m_child_nodes.begin();
      i!=m_child_nodes.end(); ++i) {
    if (*i!=NULL) {
      (*i)->process(bufsize);
    }
  }
}

void graph_node::clear() {
  for(unsigned int n=0; n<m_child_nodes.size(); n++) {
    m_child_nodes[n]=NULL;
  }
}

void graph_node::set_child(unsigned int num, graph_node *s) {
  if(num<m_child_nodes.size()) {
    m_child_nodes[num]=s;
  }
}

bool graph_node::child_exists(unsigned int num) {
  return get_child(num)!=NULL;
}

graph_node* graph_node::get_child(unsigned int num) {
  if(num<m_child_nodes.size()) {
    return m_child_nodes[num];
  }
  return NULL;
}

sample &graph_node::get_input(unsigned int num) {
  assert(get_child(num)!=NULL);
  return get_child(num)->get_output();
}

float graph_node::getCVvalue() {
  if (is_terminal()) return get_value();
  else return get_output()[(unsigned int)0];
}
