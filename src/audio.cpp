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

#include <string>
#include "audio.h"

using namespace std;

audio_device::audio_device(const string &clientname, u32 samplerate, u32 buffer_size) :
    left_out(buffer_size),
    right_out(buffer_size),
    left_in(buffer_size),
    right_in(buffer_size)
{
    PortAudioClient::DeviceOptions opt;
    opt.BufferSize = buffer_size;
    opt.NumBuffers = 2;
    opt.Samplerate = samplerate;
    opt.InChannels = 2;
    opt.OutChannels = 2;

    m_client.Attach(clientname,opt);
    m_client.SetOutputs(left_out.GetBuffer(), right_out.GetBuffer());
//    m_client.SetInputs(left_in.GetBuffer(), right_in.GetBuffer());

}

void run_graph(void *c, unsigned int size) {
    audio_device *a=(audio_device *)c;
    a->left_out.Zero();
    a->right_out.Zero();
    a->m_graph->Process(size, a->left_out, a->right_out);
}

void audio_device::start_graph(Graph *graph) {
    m_graph = graph;
    m_client.SetCallback(run_graph,this);
}
