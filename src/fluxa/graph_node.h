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
#include <math.h>
#include "sample.h"

#ifndef GRAPHNODE
#define GRAPHNODE

using namespace std;
using namespace spiralcore;

/////////////////////////////////////////////////
/////////////////////////////////////////////////

class graph_node
{
 public:
  graph_node(unsigned int numinputs);
  virtual ~graph_node();
	
  virtual void trigger(float time) {}
  virtual void process(unsigned int bufsize)=0;
  virtual float get_value() { return 0; }
  virtual bool is_terminal() { return false; }
  virtual sample &get_output() { return m_output; }
  virtual void clear();
	
  void trigger_children(float time);
  // so we can turn of the root when we are recycled
  void register_root(unsigned int root_id);
  void process_children(unsigned int bufsize);
  void set_child(unsigned int num, graph_node *s);
  bool child_exists(unsigned int num);
  graph_node* get_child(unsigned int num);
  sample &get_input(unsigned int num);
  float getCVvalue();
  unsigned int get_root_id() { return m_root_id; }
	
 protected:
  sample m_output;
	
 private:
  vector<graph_node*> m_child_nodes;
  unsigned int m_root_id;
};

#endif
