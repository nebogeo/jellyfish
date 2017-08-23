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

#include <stdio.h>
#include <limits.h>

#include "portaudio_client.h"

///////////////////////////////////////////////////////

portaudio_client::portaudio_client() :
  m_buffer_size(0),
  m_sample_rate(44100),
  m_attached(false),
  m_right_data(NULL),
  m_left_data(NULL),
  m_right_in_data(NULL),
  m_left_in_data(NULL),
  run_context(NULL)
{
}

/////////////////////////////////////////////////////////////////////////////////////////////

portaudio_client::~portaudio_client() {
  cerr<<"portaudio destructor "<<this<<endl;
  detach();
}

/////////////////////////////////////////////////////////////////////////////////////////////

bool portaudio_client::attach(const string &client_name, const device_options &dopt) {
  if (m_attached) return true;

  PaError err;
  err = Pa_Initialize();
  if( err != paNoError ) {
    cerr<<"could not init portaudio_client"<<endl;
    Pa_Terminate();
    fprintf( stderr, "an error occured while using the portaudio stream\n" );
    fprintf( stderr, "error number: %d\n", err );
    fprintf( stderr, "error message: %s\n", Pa_GetErrorText( err ) );
  }

  PaDeviceIndex output_device_num = Pa_GetDefaultOutputDevice(); 
  PaDeviceIndex input_device_num = Pa_GetDefaultInputDevice(); 
  
  PaStreamParameters output_parameters;
  output_parameters.device = dopt.device_num;
  if (output_parameters.device == paNoDevice) {
    cerr<<"error: no default output device."<<endl;
  }
  output_parameters.channelCount = 2;  
  output_parameters.sampleFormat = paFloat32;
  output_parameters.suggestedLatency = Pa_GetDeviceInfo( output_parameters.device )->defaultLowOutputLatency;
  output_parameters.hostApiSpecificStreamInfo = NULL;

  cerr<<"Connecting to "<<Pa_GetDeviceInfo( output_parameters.device )->name<<" for output"<<endl;
  /*
  PaStreamParameters input_parameters;
  input_parameters.device = input_device_num;
  if (input_parameters.device == paNoDevice) {
    cerr<<"error: no default input device."<<endl;
  }
  input_parameters.channelCount = 1;    
  input_parameters.sampleFormat = paFloat32; 
  input_parameters.suggestedLatency = Pa_GetDeviceInfo( input_parameters.device )->defaultLowInputLatency;
  input_parameters.hostApiSpecificStreamInfo = NULL;

  cerr<<"Connecting to "<<Pa_GetDeviceInfo( input_parameters.device )->name<<" for input"<<endl;
  */
  err = Pa_OpenStream(&m_stream,
		      NULL, //&input_parameters,
		      &output_parameters,
		      dopt.samplerate,
		      dopt.buffer_size,
		      paClipOff,
		      process,
		      this);

  if(err != paNoError) {
    cerr<<"could not attach portaudio_client: "<<Pa_GetErrorText( err )<<endl;
    Pa_Terminate();
    return false;
  }

  err = Pa_StartStream(m_stream);

  if(err != paNoError) {
    cerr<<"could not start stream: "<<Pa_GetErrorText( err )<<endl;
    Pa_Terminate();
    return false;
  }

  m_attached=true;
  cerr<<"connected to portaudio..."<<endl;
  return true;
}

/////////////////////////////////////////////////////////////////////////////////////////////

void portaudio_client::detach() {
  cerr<<"detaching from portaudio"<<endl;
  Pa_AbortStream(m_stream);
  Pa_Terminate();
  m_attached=false;
  cerr<<"detached from portaudio"<<endl;
}

/////////////////////////////////////////////////////////////////////////////////////////////

int portaudio_client::process(const void *input_buffer, void *output_buffer,
			      unsigned long frames_per_buffer,
			      const PaStreamCallbackTimeInfo* time_info,
			      PaStreamCallbackFlags status_flags,
			      void *user_data) {
  portaudio_client *that=(portaudio_client*)user_data;

  that->m_buffer_size=frames_per_buffer;

  if(that->run_callback&&that->run_context) {
    // do the work
    that->run_callback(that->run_context, frames_per_buffer);
  }

  if (that->m_right_data && that->m_left_data) {
    float *out = (float*)output_buffer;
    for (unsigned int n=0; n<that->m_buffer_size; n++) {
      *out=that->m_left_data[n];
      if (*out>1) *out=1;
      if (*out<-1) *out=-1;
      out++;
      *out=that->m_right_data[n];
      if (*out>1) *out=1;
      if (*out<-1) *out=-1;
      out++;
    }
  }
  
  /*
  if (that->m_right_in_data && that->m_left_in_data) {
    float *in = (float*)input_buffer;
    for (unsigned int n=0; n<that->m_buffer_size; n++) {
      that->m_left_in_data[n]=*in;
      in++;
      //m_right_in_data[n]=*in;
      //in++;
    }
  }
  */

  return 0;
}
