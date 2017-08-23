// copyright (C) 2004 david griffiths <dave@pawfal.org>
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

#include "types.h"
#include "sample.h"
#include "modules.h"
#include "graph_node.h"
//#include "sampler.h"

#ifndef MODULENODES
#define MODULENODES

class terminal_node : public graph_node
{
 public:
  terminal_node(float value);
  virtual void process(unsigned int bufsize) {}
  virtual float get_value() { return m_value; }
  virtual void set_value(float s) { m_value=s; }
  virtual bool is_terminal() { return true; }
  //virtual void clear() { graph_node::clear(); m_value=0; }

 private:
  float m_value;
};

class osc_node : public graph_node
{
 public:
  osc_node(unsigned int shape, unsigned int sample_rate);
  virtual void trigger(float time);
  virtual void process(unsigned int bufsize);

 private:
  wave_table m_wave_table;
  unsigned int m_shape;
};

class KSnode : public graph_node
{
 public:
  KSnode(unsigned int sample_rate);
  virtual void trigger(float time);
  virtual void process(unsigned int bufsize);

 private:
  KS m_KS;
  unsigned int m_shape;
};

class Xfade_node : public graph_node
{
 public:
  Xfade_node();
  virtual void trigger(float time);
  virtual void process(unsigned int bufsize);
};

class hold_node : public graph_node
{
 public:
  enum type{SAMP, TRACK};

  hold_node(type t);
  virtual void trigger(float time);
  virtual void process(unsigned int bufsize);

 private:
  type m_type;
  float m_held_value, m_last_ctrl_val;

};


class ADSRnode : public graph_node
{
 public:
  ADSRnode(unsigned int sample_rate);
  virtual void trigger(float time);
  virtual void process(unsigned int bufsize);

 private:
  envelope m_envelope;
  sample m_temp;
};

class math_node : public graph_node
{
 public:
  enum type{ADD,SUB,MUL,DIV,POW};

  math_node(type t);
  virtual void trigger(float time);
  virtual void process(unsigned int bufsize);

 private:
  type m_type;
};

class filter_node : public graph_node
{
 public:
  enum type{MOOGLP,MOOGBP,MOOGHP,FORMANT};

  filter_node(type t, unsigned int samplerate);
  virtual void trigger(float time);
  virtual void process(unsigned int bufsize);

 private:
  type m_type;
  filter_wrapper m_filter;
  sample m_temp;
};

/*class sample_node : public graph_node
  {
  public:
  enum play_mode{TRIGGER,LOOP,NOTRIGGER,REV_TRIGGER,REV_LOOP,REV_NOTRIGGER};

  sample_node(unsigned int samplerate);
  virtual void trigger(float time);
  virtual void process(unsigned int bufsize);

  private:
  play_mode m_play_mode;
  sampler m_sampler;
  sample m_temp;
  };*/

class effect_node : public graph_node
{
 public:
  enum type{CRUSH,DISTORT,CLIP,DELAY};

  effect_node(type type, unsigned int samplerate);
  virtual void trigger(float time);
  virtual void process(unsigned int bufsize);

 private:
  type m_type;
  delay m_delay;
};

class pad_node : public graph_node
{
 public:
  pad_node(unsigned int sample_rate);
  virtual void trigger(float time);
  virtual void process(unsigned int bufsize);

 private:
  pad m_pad;
};

#endif
