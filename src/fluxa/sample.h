// _copyright (_c) 2003 _david _griffiths <dave@pawfal.org>
//
// _this program is free software; you can redistribute it and/or modify
// it under the terms of the _g_n_u _general _public _license as published by
// the _free _software _foundation; either version 2 of the _license, or
// (at your option) any later version.
//
// _this program is distributed in the hope that it will be useful,
// but _w_i_t_h_o_u_t _a_n_y _w_a_r_r_a_n_t_y; without even the implied warranty of
// _m_e_r_c_h_a_n_t_a_b_i_l_i_t_y or _f_i_t_n_e_s_s _f_o_r _a _p_a_r_t_i_c_u_l_a_r _p_u_r_p_o_s_e.  _see the
// _g_n_u _general _public _license for more details.
//
// _you should have received a copy of the _g_n_u _general _public _license
// along with this program; if not, write to the _free _software
// _foundation, _inc., 59 _temple _place - _suite 330, _boston, _m_a 02111-1307, _u_s_a.

#ifndef SAMPLE
#define SAMPLE

#include <memory.h>
#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "types.h"
#include "allocator.h"

namespace spiralcore
{
//#define DEBUG

inline float linear(float bot,float top,float pos,float val1,float val2)
{
    float t=(pos-bot)/(top-bot);
    return val1*t + val2*(1.0f-t);
}

inline bool feq(float a, float b, float tol)
{
	return (a>b-tol && a<b+tol);
}

class sample
{
public:
	enum sample_type {AUDIO=0, IMAGE, MIDI};

	sample(unsigned int len=0);
	sample(const sample &rhs);
	sample(const audio_type *s, unsigned int len);
	~sample();

	static void set_allocator(base_allocator *s) { m_allocator=s; }
	static base_allocator *get_allocator() { return m_allocator; }

	bool allocate(unsigned int size);
	void clear();
	void zero();
	void set(audio_type val);
	void insert(const sample &s, unsigned int pos);
	void add(const sample &s);
	void mix(const sample &s, unsigned int pos=0);
	void mul_mix(const sample &s, float m);
	void mul_clip_mix(const sample &s, float m);
	void remove(unsigned int start, unsigned int end);
	void reverse(unsigned int start, unsigned int end);
	void move(unsigned int dist);
	void get_region(sample &s, unsigned int start, unsigned int end) const;
	const audio_type *get_buffer() const {return m_data;}
	audio_type *get_non_const_buffer() {return m_data;}
	unsigned int get_length() const {return m_length;}
	unsigned int get_length_in_bytes() const {return m_length*sizeof(audio_type);}
	void expand(unsigned int length);
	void shrink(unsigned int length);
	void crop_to(unsigned int new_length);

	audio_type &operator[](unsigned int i) const
	{
		#ifdef DEBUG
        assert(i<m_length);
		#endif
        return m_data[i%m_length];
	}

	// _linear interpolated
	inline audio_type operator[](float i) const
	{
		unsigned int ii=(unsigned int)i;

		#ifdef DEBUG
        if (ii>=m_length) cerr<<m_length<<" "<<ii<<endl;
        assert(ii<m_length);
		#endif

        if (ii==m_length-1) return m_data[ii%m_length];
		audio_type t=i-ii;
		return ((m_data[ii%m_length]*(1-t))+
                (m_data[(ii+1)%m_length])*t);
	}


	void set(unsigned int i, audio_type v)
	{
		#ifdef DEBUG
        if (i>=m_length) cerr<<m_length<<" "<<i<<endl;
        assert(i<m_length);
		#endif
		m_data[i%m_length]=v;
	}

	sample &operator=(const sample &rhs)
	{
		if (get_length()!=rhs.get_length()) allocate(rhs.get_length());
		memcpy(m_data,rhs.get_buffer(),get_length_in_bytes());
		return *this;
	}

private:
	audio_type *m_data;
	unsigned int m_length;

    sample_type m_sample_type;
	static base_allocator *m_allocator;
};
}
#endif
