// Copyright (C) 2015 David Griffiths <dave@pawfal.org>
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#include "core/types.h"
#include "fluxa/graph.h"
#include "fluxa/sample.h"

#define USE_JACK

#ifdef USE_JACK
#include "audio/JackClient.h"
#else
#include "audio/portaudio_client.h"
#endif

#include <string>

class graph;

namespace spiralcore {

class audio_device {
public:
  audio_device(const string &clientname, u32 device, u32 samplerate, u32 buffer_size);

  void start_graph(graph *graph);
  void reset_watchdog_counter() { m_watchdog_counter=0; }

  void start_recording(std::string filename);
  void stop_recording();
  void maybe_record();

  void start_audio();
  void check_audio();
  
  sample left_out;
  sample right_out;
  sample left_in;
  sample right_in;
  graph *m_graph;

#ifndef USE_JACK
  portaudio_client *m_client;
#endif

  static void save_sample(const std::string &filename, const sample s);

  eq m_left_eq;
  eq m_right_eq;
  compressor m_left_comp;
  compressor m_right_comp;

 private:
  bool m_recording;
  std::string m_record_filename;
  sample m_record_buffer_left;
  sample m_record_buffer_right;
  u32 m_record_counter;
  u32 m_watchdog_counter;
  string m_client_name;

#ifndef USE_JACK
  portaudio_client::device_options m_opt;
#endif

};

}
