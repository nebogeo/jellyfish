// copyright (C) 2003 david griffiths <dave@pawfal.org>
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
#include "time.h"

#ifndef NE_EVENT
#define NE_EVENT

namespace spiralcore
{

class event
{
public:
	event() :
	ID(0),
	frequency(440.0f),
	slide_frequency(0.0f),
	volume(1.0f),
	pan(0.0f),
	position(0),
	channel(0),
	note_num(0),
	message(0)
	{}
	
	int ID;            // the currently playing sample, or voice
	float32 frequency; // freq
	float32 slide_frequency; // slide dest freq
	float32 volume;    // or velocity
	float32 pan;       // stereo pan
	float32 position;  // sample position start->end 0->1
	int channel;       // output channel for this event
	int note_num;
	char message;      // used for charscore message passing
	time time_stamp;    // when to do this event
};

}

#endif
