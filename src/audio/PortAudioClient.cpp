// Copyright (C) 2003 David Griffiths <dave@pawfal.org>
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

#include <stdio.h>
#include <limits.h>

#include "PortAudioClient.h"

PortAudioClient*  PortAudioClient::m_Singleton  = NULL;
bool              PortAudioClient::m_Attached   = false;
long unsigned int PortAudioClient::m_BufferSize = 0;
long unsigned int PortAudioClient::m_SampleRate = 44100;
void            (*PortAudioClient::RunCallback)(void*, unsigned int BufSize)=NULL;
void             *PortAudioClient::RunContext   = NULL;
float *PortAudioClient::m_RightData=NULL;
float *PortAudioClient::m_LeftData=NULL;
float *PortAudioClient::m_RightInData=NULL;
float *PortAudioClient::m_LeftInData=NULL;

///////////////////////////////////////////////////////

PortAudioClient::PortAudioClient()
{
}

/////////////////////////////////////////////////////////////////////////////////////////////

PortAudioClient::~PortAudioClient()
{
	Detach();
}

/////////////////////////////////////////////////////////////////////////////////////////////

bool PortAudioClient::Attach(const string &ClientName, const DeviceOptions &dopt)
{
	if (m_Attached) return true;

    PaError err;
	err = Pa_Initialize();
    if( err != paNoError )
 	{
		cerr<<"Could not init PortAudioClient"<<endl;
        Pa_Terminate();
        fprintf( stderr, "An error occured while using the portaudio stream\n" );
        fprintf( stderr, "Error number: %d\n", err );
        fprintf( stderr, "Error message: %s\n", Pa_GetErrorText( err ) );
	}

    PaStreamParameters output_parameters;
    output_parameters.device = Pa_GetDefaultOutputDevice(); /* default output device */
    if (output_parameters.device == paNoDevice) {
		cerr<<"Error: No default output device."<<endl;
    }
    output_parameters.channelCount = 2;       /* stereo output */
    output_parameters.sampleFormat = paFloat32; /* 32 bit floating point output */
    output_parameters.suggestedLatency = Pa_GetDeviceInfo( output_parameters.device )->defaultLowOutputLatency;
    output_parameters.hostApiSpecificStreamInfo = NULL;

    PaStream *stream;

    err = Pa_OpenStream(
              &stream,
              NULL, /* no input */
              &output_parameters,
              dopt.Samplerate,
              dopt.BufferSize,
              paClipOff,  /* we won't output out of range samples so don't bother clipping them */
              Process,
              NULL);

    if( err != paNoError )
	{
		cerr<<"Could not attach PortAudioClient: "<<Pa_GetErrorText( err )<<endl;
		Pa_Terminate();
		return false;
	}

	err = Pa_StartStream(stream);

	if( err != paNoError )
	{
		cerr<<"Could not start stream: "<<Pa_GetErrorText( err )<<endl;
		Pa_Terminate();
		return false;
	}

	m_Attached=true;
	cerr<<"connected to portaudio..."<<endl;
	return true;
}

/////////////////////////////////////////////////////////////////////////////////////////////

void PortAudioClient::Detach()
{
    cerr<<"Detaching from portaudio"<<endl;
    Pa_Terminate();
    m_Attached=false;
}

/////////////////////////////////////////////////////////////////////////////////////////////

int PortAudioClient::Process(const void *inputBuffer, void *outputBuffer,
                             unsigned long framesPerBuffer,
                             const PaStreamCallbackTimeInfo* timeInfo,
                             PaStreamCallbackFlags statusFlags,
                             void *userData)
{
	m_BufferSize=framesPerBuffer;

	if(RunCallback&&RunContext)
	{
		// do the work
		RunCallback(RunContext, framesPerBuffer);
	}

	if (m_RightData && m_LeftData)
	{
		float *out = (float*)outputBuffer;
		for (unsigned int n=0; n<m_BufferSize; n++)
		{
			*out=m_LeftData[n];
			out++;
			*out=m_RightData[n];
			out++;
		}
	}

	if (m_RightInData && m_LeftInData)
	{
		float *in = (float*)inputBuffer;
		for (unsigned int n=0; n<m_BufferSize; n++)
		{
			m_LeftInData[n]=*in;
			in++;
			m_RightInData[n]=*in;
			in++;
		}
	}
	return 0;
}
