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

#include "event.h"

#ifndef SPIRALCORE_EVENT_QUEUE
#define SPIRALCORE_EVENT_QUEUE

static const int EVENT_QUEUE_SIZE = 256;

namespace spiralcore
{

  // no mallocs, so a bit of diy memory allocation

class event_queue
{
 public:
  event_queue();
  ~event_queue();
  
  bool add(const event &e);
  
  // you should keep calling this function for the specified
  // time slice until it returns false
  bool get(time from, time till, event &e);
  
 private:

  struct queue_item
  {
  queue_item() : m_is_empty(true) {}
    bool m_is_empty;
    event m_event;
  };

  queue_item m_queue[EVENT_QUEUE_SIZE]; 
};

}

#endif
