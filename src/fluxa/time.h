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

namespace spiralcore
{

class time
{
 public:
  time();
  time(int s, int f) : seconds(s),fraction(f) {}
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
  void set_fraction(double s) { fraction = (int)(s*(double)UINT_MAX); }
  bool is_empty() { return (!seconds && !fraction); }
  double get_difference(const time& other);

  unsigned int seconds;
  unsigned int fraction;
};

}

#endif
