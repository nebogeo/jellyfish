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

#include <assert.h>
#include <math.h>
#include <iostream>
#include "time.h"

using namespace spiralcore;

// got this and usec2ntp from http://www.openmash.org/lxr/source/rtp/ntp-time.h
static const unsigned long GETTIMEOFDAY_TO_NTP_OFFSET = 2208988800UL;

// convert microseconds to fraction of second * 2^32 (i.e., the lsw of
// a 64-bit ntp timestamp).  this routine uses the factorization
// 2^32/10^6 = 4096 + 256 - 1825/32 which results in a max conversion
// error of 3 * 10^-7 and an average error of half that.
unsigned int usec2ntp(unsigned int usec)
{
  unsigned int t = (usec * 1825) >> 5;
  return ((usec << 12) + (usec << 8) - t);
}

time::time() :
  seconds(0),
  fraction(0)
{

}

void time::set_to_now()
{
  timeval tv;
  gettimeofday(&tv,0);
  set_from_posix(tv);
}

void time::set_from_posix(timeval tv)
{
  // gettimeofday epoch is 00:00:00 UTC, january 1, 1970
  // ntp (what we're basing time on for OSC compat) epoch is
  // 00:00:00 UTC, january 1, 1900, so we need to convert...
  seconds = (unsigned int)tv.tv_sec + GETTIMEOFDAY_TO_NTP_OFFSET;
  fraction = usec2ntp(tv.tv_usec);
}

void time::inc_by_sample(unsigned long samples, unsigned long samplerate)
{
  (*this)+=samples/(double)samplerate;
}

spiralcore::time &time::operator+=(double s)
{
  unsigned int secs = (unsigned int)floor(s);
  seconds += secs;
  double frac = s-secs;
  // overflow? (must do this better)
  if (frac+fraction*ONE_OVER_UINT_MAX>1.0f) seconds++;
  fraction += (unsigned int)(frac*UINT_MAX);
  return *this;
}

double time::get_difference(const time& other)
{
  double secs_diff = (long)seconds-(long)other.seconds;
  double secs_frac = fraction*ONE_OVER_UINT_MAX;
  secs_frac-=other.fraction*ONE_OVER_UINT_MAX;
  return secs_diff+secs_frac;
}

bool time::operator<(const time& other)
{
  if (seconds<other.seconds) return true;
  else if (seconds==other.seconds && fraction<other.fraction) return true;
  return false;
}

bool time::operator>(const time& other)
{
  if (seconds>other.seconds) return true;
  else if (seconds==other.seconds && fraction>other.fraction) return true;
  return false;
}

bool time::operator<=(const time& other)
{
  if (seconds<other.seconds|| (seconds==other.seconds && fraction==other.fraction)) return true;
  else if (seconds==other.seconds && fraction<other.fraction) return true;
  return false;
}

bool time::operator>=(const time& other)
{
  if (seconds>other.seconds || (seconds==other.seconds && fraction==other.fraction)) return true;
  else if (seconds==other.seconds && fraction>other.fraction) return true;
  return false;
}

bool time::operator==(const time& other)
{
  if (seconds==other.seconds && fraction==other.fraction) return true;
  return false;
}

void time::print() const
{
    std::cerr<<seconds<<":"<<get_fraction()<<" ("<<fraction<<")"<<std::endl;
}
