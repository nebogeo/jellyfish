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

#include <sys/time.h>
#include <limits.h>

#ifndef SPIRALCORE_TIME
#define SPIRALCORE_TIME

static const double ONE_OVER_UINT_MAX = 1.0/UINT_MAX;

// This is the deal with time
// --------------------------
// 
// We have two types of time dealt with here, unix timestamps
// and ntp timestamps - this class is an ntp timestamp but is derived
// from gettimeofday which produces unix timestamps.
//
// Unix timestamps
// ---------------
// Unix timestamps can either be 32 or 64 bits, but both correspond 
// to seconds and microseconds (millioth of a second) since 1970. Not
// sure what the benefit of microseconds being 64 bit is, other than
// symmetry?
// 
// NTP timestamps
// --------------
// These are 2 X 32 bits, seconds and fractions of a second (using the
// full unsigned 32 bit int range) from 1900, we use this for OSC
// compatibility

namespace spiralcore
{

class time
{
 public:
  time();
  time(unsigned int s, unsigned int f) : seconds(s),fraction(f) {}
  void set_to_now();
  void set_from_posix(timeval tv);
  void inc_by_sample(unsigned long samples, unsigned long samplerate);
  bool operator<(const time& other);
  bool operator>(const time& other);
  bool operator<=(const time& other);
  bool operator>=(const time& other);
  bool operator==(const time& other);
  time &operator+=(double s);
  void print() const;
  double get_fraction() const { return fraction*ONE_OVER_UINT_MAX; }
  void set_fraction(double s) { fraction = (unsigned int)(s*(double)UINT_MAX); }
  bool is_empty() { return (!seconds && !fraction); }
  double get_difference(const time& other);

  unsigned int seconds;
  unsigned int fraction;
};

}

#endif
