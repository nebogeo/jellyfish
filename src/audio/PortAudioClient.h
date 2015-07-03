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

#include <map>
#include <vector>
#include <string>
#include <iostream>
#include <portaudio.h>

using namespace std;

#ifndef PA_CLIENT
#define PA_CLIENT

class PortAudioClient
{
public:
	PortAudioClient();
	~PortAudioClient();

	class DeviceOptions
	{
		public:
		enum type {READ,WRITE,READWRITE};
		unsigned int BufferSize;
		unsigned int NumBuffers;
		unsigned int Samplerate;
		unsigned int InChannels;
		unsigned int OutChannels;
	};

	bool   Attach(const string &ClientName, const DeviceOptions &dopt);
	void   Detach();
	bool   IsAttached()                   { return m_Attached; }
	void   SetCallback(void(*Run)(void*, unsigned int),void *Context) { RunCallback=Run; RunContext=Context; }
	void   SetOutputs(float *l, float *r) { m_LeftData=l; m_RightData=r; }
	void   SetInputs(float *l, float *r) { m_LeftInData=l; m_RightInData=r; }

protected:

	static int  Process(const void *inputBuffer, void *outputBuffer,
                             unsigned long framesPerBuffer,
                             const PaStreamCallbackTimeInfo* timeInfo,
                             PaStreamCallbackFlags statusFlags,
                             void *userData);

private:

	static PortAudioClient*   m_Singleton;
	static long unsigned int  m_BufferSize;
	static long unsigned int  m_SampleRate;
	static bool               m_Attached;

	static float *m_RightData;
	static float *m_LeftData;
	static float *m_RightInData;
	static float *m_LeftInData;

	static void(*RunCallback)(void *, unsigned int);
	static void *RunContext;
};

#endif
