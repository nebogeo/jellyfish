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

#include "event_queue.h"

using namespace spiralcore;

event_queue::event_queue()
{
}

event_queue::~event_queue()
{
}

bool event_queue::add(const event &e)
{
  for (int i=0; i<EVENT_QUEUE_SIZE; i++)
    {
      if (m_queue[i].m_is_empty)
	{
	  m_queue[i].m_event=e;
	  m_queue[i].m_is_empty=false;
	  return true;
	}
    }
  
  return false;
}

bool event_queue::get(time from, time till, event &e)
{
  for (int i=0; i<EVENT_QUEUE_SIZE; i++)
    {
      if (!m_queue[i].m_is_empty && m_queue[i].m_event.time_stamp>from &&
	  m_queue[i].m_event.time_stamp<till)
	{
	  e=m_queue[i].m_event;      // return this one
	  m_queue[i].m_is_empty=true; // delete from the queue
	  return true;
	}
    }
  return false;
}

