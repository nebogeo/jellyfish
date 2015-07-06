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

#include "types.h"
#include "sample.h"
#include <stdlib.h>

#ifndef MODULES
#define MODULES

using namespace spiralcore;
using namespace std;

static const int NUM_TABLES = 9;
static const int DEFAULT_TABLE_LEN = 1024;
static const int FILTER_GRANULARITY = 10;
static const float PI=3.141592654;
static const float RAD=(PI/180.0)*360.0;

float rand_range(float L, float H);
void crush(sample &buf, float freq, float bits);
void distort(sample &buf, float amount);
void moving_distort(sample &buf, const sample &amount);
void hard_clip(sample &buf, float level);
void moving_hard_clip(sample &buf, const sample &level);

class module
{
public:
	module(int sample_rate) : m_sample_rate(sample_rate) {}
	virtual ~module() {}

	virtual void process(unsigned int buf_size, sample &in) {}
	virtual void trigger(float time, float pitch, float vol) {}
	virtual void reset() {}
protected:
	int m_sample_rate;
};


class wave_table : public module
{
public:
	wave_table(int sample_rate);
	~wave_table() {}

	typedef char type;
	enum {SINE,SQUARE,SAW,REVSAW,TRIANGLE,PULSE1,PULSE2,NOISE,PINKNOISE};

	virtual void process(unsigned int buf_size, sample &in);
	virtual void processFM(unsigned int buf_size, sample &in, const sample &pitch);
	void simple_process(unsigned int buf_size, sample &in);
	virtual void trigger(float time, float pitch, float slidepitch, float vol);
	virtual void reset();

	static void write_waves();

	void set_volume(float s)   { m_volume=s; }
	void set_type(type s)      { m_type=s; }
	void set_octave(int s)     { m_octave=s; }
	void set_fine_freq(float s) { m_fine_freq=s; }
	void set_slide_length(float s) { m_slide_length=s; }

private:

	float m_pitch;
	float m_target_pitch;
	float m_volume;
	int   m_note;
	float m_cycle_pos;
	type  m_type;
	int   m_octave;
	float m_fine_freq;
	float m_slide_time;
	float m_slide_length;
	float m_time_per_sample;
	float m_table_per_sample;

	static sample m_table[NUM_TABLES];
	static unsigned int m_table_length;
};

class simple_wave : public module
{
public:
	simple_wave(int sample_rate);
	~simple_wave() {}

	virtual void process(unsigned int buf_size, sample &in);
	virtual void trigger(float time, float pitch, float slidepitch, float vol);
	virtual void reset();

	void write_waves();

	void set_volume(float s)   { m_volume=s; }
	void set_fine_freq(float s) { m_fine_freq=s; }

private:

	float m_pitch;
	float m_slide_pitch;
	float m_volume;
	int   m_note;
	float m_cycle_pos;
	float m_fine_freq;

	//\todo make these static??!!
	sample m_table;
	unsigned int m_table_length;
};

class envelope : public module
{
public:
	envelope(int sample_rate);
	virtual ~envelope() {}

	virtual void process(unsigned int buf_size, sample &CV, bool smooth=true);
	virtual void trigger(float time, float pitch, float vol);
	virtual void reset();

	void set_attack(float s)  { m_attack=s; }
	void set_decay(float s)   { m_decay=s; }
	void set_sustain(float s) { m_sustain=s; }
	void set_release(float s) { m_release=s; }
	void set_volume(float s)  { m_volume=s; }

protected:
	bool   m_trigger;
	float  m_t;
	float m_attack;
	float m_decay;
	float m_sustain;
	float m_release;
	float m_volume;
	float m_sample_time;
	float m_current;

};

class simple_envelope : public module
{
public:
	simple_envelope(int sample_rate);
	virtual ~simple_envelope() {}

	virtual void process(unsigned int buf_size, sample &in, sample &CV, bool smooth=true);
	virtual void trigger(float time, float pitch, float vol);
	virtual void reset();

	void set_decay(float s)   { m_decay=s; }
	void set_volume(float s)  { m_volume=s; }

protected:
	bool  m_trigger;
	float m_t;
	float m_decay;
	float m_volume;
	float m_sample_time;
	float m_current;
};

class moog_filter : public module
{
public:
	moog_filter(int sample_rate);
	virtual ~moog_filter() {}

	virtual void process(unsigned int buf_size, sample &in, sample *cutoffCV, sample *LPFout, sample *BPFout, sample *HPFout);
	virtual void reset();

	void set_cutoff(float s) { cutoff=s; }
	void set_resonance(float s) { if (s<0.5 && s>=0.0) resonance=s; }

    // only used by ks, as I couldn't figure out the cyclic buffer stuff :(
    inline float process_single(float in)
    {
        float Q=0;
        fc = cutoff;
        fc*=0.25;
        if (fc<0) fc=0;
        else if (fc>1) fc=1;

        q = 1.0f - fc;
        p = fc + 0.8f * fc * q;
        f = p + p - 1.0f;
        Q = resonance*6-3;
        q = Q + (1.0f + 0.5f * q * (1.0f - q + 5.6f * q * q));

        // say no to denormalisation!
        in+=(rand()%1000)*0.000000001;

        in -= q * b4;

        if (in>1) in=1;
        if (in<-1) in=-1;

        t1 = b1; b1 = (in + b0) * p - b1 * f;
        t2 = b2; b2 = (b1 + t1) * p - b2 * f;
        t1 = b3; b3 = (b2 + t2) * p - b3 * f;
        b4 = (b3 + t1) * p - b4 * f;
        b4 = b4 - b4 * b4 * b4 * 0.166667f;

        b0 = in;

        return b4;
    }

protected:
	float cutoff, resonance;

	float fs, fc;
	float f,p,q;
	float b0,b1,b2,b3,b4;
	float t1,t2;

	float in1,in2,in3,in4,out1,out2,out3,out4;
};

class formant_filter : public module
{
public:
 	formant_filter(int sample_rate);
	virtual ~formant_filter() {}
	virtual void process(unsigned int buf_size, sample &in, sample *cutoffCV, sample &out);
	virtual void reset();

	void set_cutoff(float s) { m_vowel=s; }
	void set_resonance(float s) {}

private:
	float m_vowel;
	double memory[5][10];
};

// a wrapper for the other filters
class filter_wrapper : public module
{
public:
	filter_wrapper(int sample_rate);
	virtual ~filter_wrapper() {}

	enum type {MOOG_LO,MOOG_BAND,MOOG_HI,FORMANT};

	void set_type(type s) { m_type=s; }
	void set_cutoff(float s) { m_moog_filter.set_cutoff(s); m_formant_filter.set_cutoff(s); }
	void set_resonance(float s) { m_moog_filter.set_resonance(s); m_formant_filter.set_resonance(s); }

	virtual void process(unsigned int buf_size, sample &in, sample &cutoffCV, sample &out);
	virtual void process(unsigned int buf_size, sample &in, sample &out);
	virtual void reset();

private:
	moog_filter m_moog_filter;
	formant_filter m_formant_filter;
	type m_type;
};

class delay : public module
{
public:
	delay(int sample_rate);
	virtual ~delay() {}

	virtual void process(unsigned int buf_size, sample &in, sample &delayCV, sample &feedbackCV, sample &out);
	virtual void process(unsigned int buf_size, sample &in, sample &out);
	virtual void reset();

	void set_delay(float s) { m_delay=s; }
	void set_feedback(float s) { m_feedback=s; }

protected:
	float m_delay, m_feedback;
	unsigned int m_position;
	sample m_buffer;
};

class eq : public module
{
public:
	eq(int sample_rate);
	virtual ~eq() {}

	virtual void process(unsigned int buf_size, sample &in);

	void set_low(float s) { m_low=s; }
	void set_mid(float s) { m_mid=s; }
	void set_high(float s) { m_high=s; }

protected:
	// filter #1 (low band)
	float  lf;       // frequency
	float  f1p0;     // poles ...
	float  f1p1;
	float  f1p2;
	float  f1p3;

	// filter #2 (high band)
	float  hf;       // frequency
	float  f2p0;     // poles ...
	float  f2p1;
	float  f2p2;
	float  f2p3;

	// sample history buffer
	float  sdm1;     // sample data minus 1
	float  sdm2;     //                   2
	float  sdm3;     //                   3

	float m_low;
	float m_mid;
	float m_high;

};


class compressor : public module
{
public:
	compressor(int sample_rate);
	virtual ~compressor() {}

	virtual void process(unsigned int buf_size, sample &in);

	void set_attack(float s) { tatt=s*1e-3; }
	void set_release(float s) { trel=s*1e-3; }
	void set_threshold(float s) { threshold=s; }
	void set_slope(float s) { slope=s; }

protected:

	float threshold;  // threshold (percents)
	float slope;      // slope angle (percents)
    int   sr;         // sample rate (smp/sec)
    float tla;        // lookahead  (ms)
    float twnd;       // window time (ms)
    float tatt;       // attack time  (ms)
    float trel;       // release time (ms)
};

class KS : public module
{
public:
	KS(int sample_rate);
	virtual ~KS() {}

	virtual void process(unsigned int buf_size, sample &out);
	virtual void trigger(float time, float pitch, float slidepitch, float vol);
	virtual void reset();

    void set_cutoff(float s) { m_filter.set_cutoff(s); }
    void set_resonance(float s) { m_filter.set_resonance(s); }

protected:
	float m_delay, m_feedback;
	unsigned int m_position;
	sample m_buffer;
    moog_filter m_filter;
};

class pad : public module
{
public:
	pad(int sample_rate);
	~pad() {}

	virtual void process(unsigned int buf_size, sample &in);
	virtual void trigger(float time, float pitch, float slidepitch, float vol);
	virtual void reset();

	void write_waves();

	void set_volume(float s)   { m_volume=s; }
	void set_gap(float s)   { m_gap=s; }
    void set_cutoff(float s) { m_filter.set_cutoff(s); }
    void set_resonance(float s) { m_filter.set_resonance(s); }

private:

    moog_filter m_filter;
    float m_gap;
    float m_state;
    float m_table_per_sample;
	float m_pitch;
	float m_volume;
	int   m_note;
	float m_cycle_pos;
    unsigned int m_write_pos;
	sample m_table;
	unsigned int m_table_length;
};


#endif
