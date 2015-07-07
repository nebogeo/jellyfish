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

#include "module_nodes.h"

#include <iostream>
using namespace std;

terminal_node::terminal_node(float value):
graph_node(0),
m_value(value)
{
}

osc_node::osc_node(unsigned int shape, unsigned int sample_rate):
graph_node(1),
m_wave_table(sample_rate),
m_shape(shape)
{
	m_wave_table.set_type(m_shape);
}

void osc_node::trigger(float time)
{
	trigger_children(time);

	float freq=440;
	if (child_exists(0) && get_child(0)->is_terminal())
	{
		freq=get_child(0)->get_value();
	}
	m_wave_table.trigger(time, freq, freq, 1);
}

void osc_node::process(unsigned int bufsize)
{
	if (bufsize>(unsigned int)m_output.get_length())
	{
        cerr<<"increase bufsize:"<<m_output.get_length()<<" "<<bufsize<<endl;
		m_output.allocate(bufsize);
	}
	process_children(bufsize);

	if (child_exists(0) && !get_child(0)->is_terminal())
	{
		m_wave_table.processFM(bufsize, m_output, get_input(0));
	}
	else
	{
		m_wave_table.process(bufsize, m_output);
	}
}

ADSRnode::ADSRnode(unsigned int sample_rate):
graph_node(4),
m_envelope(sample_rate)
{
}

void ADSRnode::trigger(float time)
{
	trigger_children(time);

	if (child_exists(0)) m_envelope.set_attack(get_child(0)->getCVvalue());
	if (child_exists(1)) m_envelope.set_decay(get_child(1)->getCVvalue());
	if (child_exists(2)) m_envelope.set_sustain(get_child(2)->getCVvalue());
	if (child_exists(3)) m_envelope.set_release(get_child(3)->getCVvalue());

	m_envelope.trigger(time, 0, 1);
}

void ADSRnode::process(unsigned int bufsize)
{
	if (bufsize>(unsigned int)m_output.get_length())
	{
		m_output.allocate(bufsize);
	}

	process_children(bufsize);
	m_envelope.process(bufsize, m_output);
}

math_node::math_node(type t):
graph_node(2),
m_type(t)
{
}

void math_node::trigger(float time)
{
	trigger_children(time);
}

void math_node::process(unsigned int bufsize)
{
	if (bufsize>(unsigned int)m_output.get_length())
	{
		m_output.allocate(bufsize);
	}

	process_children(bufsize);

	if (child_exists(0) && child_exists(1))
	{
		if (get_child(0)->is_terminal() && get_child(1)->is_terminal())
		{
			float value=0;
			float v0 = get_child(0)->get_value();
			float v1 = get_child(1)->get_value();

			switch(m_type)
			{
				case ADD: value=v0+v1; break;
				case SUB: value=v0-v1; break;
				case MUL: value=v0*v1; break;
				case DIV: if (v1!=0) value=v0/v1; break;
				case POW: if (v0!=0 || v1>0) value=powf(v0,v1); break;
			};

			for (unsigned int n=0; n<bufsize; n++) m_output[n]=value;
		}
		else if (get_child(0)->is_terminal() && !get_child(1)->is_terminal())
		{
			float v0 = get_child(0)->get_value();

			switch(m_type)
			{
				case ADD: for (unsigned int n=0; n<bufsize; n++) m_output[n]=v0+get_child(1)->get_output()[n]; break;
				case SUB: for (unsigned int n=0; n<bufsize; n++) m_output[n]=v0-get_child(1)->get_output()[n]; break;
				case MUL: for (unsigned int n=0; n<bufsize; n++) m_output[n]=v0*get_child(1)->get_output()[n]; break;
				case DIV:
				{
					for (unsigned int n=0; n<bufsize; n++)
					{
						if (get_child(1)->get_output()[n]!=0)
						{
							m_output[n]=v0/get_child(1)->get_output()[n];
						}
					}
				}
				break;
				case POW:
					for (unsigned int n=0; n<bufsize; n++)
					{
						if (v0!=0 && get_child(1)->get_output()[n]>0)
						{
							m_output[n]=powf(v0,get_child(1)->get_output()[n]);
						}
					}
				break;
			};
		}
		else if (!get_child(0)->is_terminal() && get_child(1)->is_terminal())
		{
			float v1 = get_child(1)->get_value();

			switch(m_type)
			{
				case ADD: for (unsigned int n=0; n<bufsize; n++) m_output[n]=get_child(0)->get_output()[n]+v1; break;
				case SUB: for (unsigned int n=0; n<bufsize; n++) m_output[n]=get_child(0)->get_output()[n]-v1; break;
				case MUL: for (unsigned int n=0; n<bufsize; n++) m_output[n]=get_child(0)->get_output()[n]*v1; break;
				case DIV:
				{
					if (v1!=0)
					{
						for (unsigned int n=0; n<bufsize; n++)
						{
							m_output[n]=get_child(0)->get_output()[n]/v1;
						}
					}
				}
				break;
				case POW:
					for (unsigned int n=0; n<bufsize; n++)
					{
						if (get_child(0)->get_output()[n]!=0 && v1>0)
						{
							m_output[n]=powf(get_child(0)->get_output()[n],v1);
						}
					}
				break;
			};
		}
		else
		{
			switch(m_type)
			{
				case ADD:
				{
					for (unsigned int n=0; n<bufsize; n++)
					{
						m_output[n]=get_child(0)->get_output()[n]+get_child(1)->get_output()[n];
					}

				} break;
				case SUB:
				{
					for (unsigned int n=0; n<bufsize; n++)
					{
						m_output[n]=get_child(0)->get_output()[n]-get_child(1)->get_output()[n];
					}
				} break;
				case MUL:
				{
					for (unsigned int n=0; n<bufsize; n++)
					{
						m_output[n]=get_child(0)->get_output()[n]*get_child(1)->get_output()[n];
					}
				} break;
				case DIV:
				{
					for (unsigned int n=0; n<bufsize; n++)
					{
						if (get_child(1)->get_output()[n]!=0)
						{
							m_output[n]=get_child(0)->get_output()[n]/get_child(1)->get_output()[n];
						}
					}
				}
				break;
				case POW:
				{
					for (unsigned int n=0; n<bufsize; n++)
					{
						if (get_child(0)->get_output()[n]!=0 && get_child(1)->get_output()[n]>0)
						{
							m_output[n]=powf(get_child(0)->get_output()[n],get_child(1)->get_output()[n]);
						}
					}
				} break;
			};
		}
	}
}

filter_node::filter_node(type t, unsigned int samplerate):
graph_node(3),
m_type(t),
m_filter(samplerate)
{
	switch(m_type)
	{
		case MOOGLP: m_filter.set_type(filter_wrapper::MOOG_LO); break;
		case MOOGBP: m_filter.set_type(filter_wrapper::MOOG_BAND); break;
		case MOOGHP: m_filter.set_type(filter_wrapper::MOOG_HI); break;
		case FORMANT: m_filter.set_type(filter_wrapper::FORMANT); break;
	};
}

void filter_node::trigger(float time)
{
	trigger_children(time);
}

void filter_node::process(unsigned int bufsize)
{
	if (bufsize>(unsigned int)m_output.get_length())
	{
		m_output.allocate(bufsize);
	}

	process_children(bufsize);

	if (child_exists(0) && !get_child(0)->is_terminal() && child_exists(1) && child_exists(2))
	{
		float r=get_child(2)->get_value();
		if (r>=0 && r<0.5) m_filter.set_resonance(r);

		if (get_child(1)->is_terminal())
		{
			float c=get_child(1)->get_value();
			if (c>=0 && c<1) m_filter.set_cutoff(c);

			m_filter.process(bufsize, get_input(0), m_output);
		}
		else
		{
			m_filter.process(bufsize, get_input(0), get_input(1), m_output);
		}
	}
}

/*sample_node::sample_node(unsigned int samplerate):
graph_node(2),
m_play_mode(TRIGGER),
m_sampler(samplerate)
{
}

void sample_node::trigger(float time)
{
	trigger_children(time);

	if (child_exists(0))
	{
		event e;
		e.ID = (int)get_child(0)->getCVvalue();
		e.frequency = get_child(1)->getCVvalue();
		m_sampler.play(time, e);
	}
}

void sample_node::process(unsigned int bufsize)
{
	if (bufsize>(unsigned int)m_output.get_length())
	{
		m_output.allocate(bufsize);
		m_temp.allocate(bufsize);
	}

	process_children(bufsize);
	m_output.zero();
	m_sampler.process(bufsize, m_output, m_temp);
}
*/
effect_node::effect_node(type type, unsigned int samplerate):
graph_node(3),
m_type(type),
m_delay(samplerate)
{
}

void effect_node::trigger(float time)
{
	trigger_children(time);
}

void effect_node::process(unsigned int bufsize)
{
	if (bufsize>(unsigned int)m_output.get_length())
	{
		m_output.allocate(bufsize);
	}

	process_children(bufsize);

    if (child_exists(0) && !get_child(0)->is_terminal() && child_exists(1))
    {
        if (m_type==CLIP)
        {
            m_output=get_input(0);
            if (get_child(1)->is_terminal())
            {
                hard_clip(m_output, get_child(1)->getCVvalue());
            }
            else
            {
                moving_hard_clip(m_output, get_input(1));
            }
        }
        else if (m_type==DISTORT)
        {
            m_output=get_input(0);
            if (get_child(1)->is_terminal())
            {
                distort(m_output, get_child(1)->getCVvalue());
            }
            else
            {
                moving_distort(m_output, get_input(1));
            }
        }
        else if (child_exists(2))
        {
            switch (m_type)
            {
                case CRUSH : m_output=get_input(0); crush(m_output, get_child(1)->getCVvalue(), get_child(2)->getCVvalue()); break;
                case DELAY :
                {
                    m_delay.set_delay(get_child(1)->getCVvalue());
                    m_delay.set_feedback(get_child(2)->getCVvalue());
                    m_delay.process(bufsize, get_input(0), m_output); break;
                }
				default :
					assert(0);
					break;
            }
		}
	}
}

KSnode::KSnode(unsigned int sample_rate):
graph_node(3),
m_KS(sample_rate)
{
}

void KSnode::trigger(float time)
{
	trigger_children(time);

	float freq=440;
	if (child_exists(0) && get_child(0)->is_terminal())
	{
		freq=get_child(0)->get_value();
	}
	//if (child_exists(1) && get_child(0)->is_terminal())
	//{
	//	freq=get_child(1)->get_value();
	//}

	m_KS.trigger(time, freq, freq, 1);
}

void KSnode::process(unsigned int bufsize)
{
	if (bufsize>(unsigned int)m_output.get_length())
	{
		m_output.allocate(bufsize);
	}
	process_children(bufsize);

	if (child_exists(1) && child_exists(2))
	{
		if (get_child(1)->is_terminal())
		{
			float c=get_child(1)->get_value();
			if (c>=0 && c<1) m_KS.set_cutoff(c);
		}
        if (get_child(2)->is_terminal())
        {
            float r=get_child(2)->get_value();
            if (r>=0 && r<0.5) m_KS.set_resonance(r);
		}
	}

	m_KS.process(bufsize, m_output);
}

Xfade_node::Xfade_node():
graph_node(3)
{
}

void Xfade_node::trigger(float time)
{
	trigger_children(time);
}

void Xfade_node::process(unsigned int bufsize)
{
	if (bufsize>(unsigned int)m_output.get_length())
	{
		m_output.allocate(bufsize);
	}
	process_children(bufsize);

	if (child_exists(0) && child_exists(1) && child_exists(2))
	{
		if (get_child(0)->is_terminal())
		{
			if (get_child(1)->is_terminal())
			{
				if (get_child(2)->is_terminal())
				{
					float value=0;
					float v0 = get_child(0)->get_value();
					float v1 = get_child(1)->get_value();
					float mix = get_child(2)->get_value();
					if (mix < -1) mix = -1;
					else if (mix > 1) mix = 1;
					mix = (0.5 + (mix * 0.5));
					value = (v0 * (1 - mix)) + (v1 * mix);

					for (unsigned int n=0; n<bufsize; n++) m_output[n]=value;
				}
				else
				{
					float v0 = get_child(0)->get_value();
					float v1 = get_child(1)->get_value();

					for (unsigned int n=0; n<bufsize; n++)
					{
						float mix = get_child(2)->get_output()[n];
						if (mix < -1) mix = -1;
						else if (mix > 1) mix = 1;
						mix = (0.5 + (mix * 0.5));

						m_output[n]=(v0 * (1 - mix)) + (v1 * mix);
					}
				}
			}
			else
			{
				if (get_child(2)->is_terminal())
				{
					float v0 = get_child(0)->get_value();
					float mix = get_child(2)->get_value();
					if (mix < -1) mix = -1;
					else if (mix > 1) mix = 1;
					mix = (0.5 + (mix * 0.5));

					for (unsigned int n=0; n<bufsize; n++)
					{

						m_output[n]=(v0 * (1 - mix)) + (get_child(1)->get_output()[n] * mix);
					}
				}
				else
				{
					float v0 = get_child(0)->get_value();

					for (unsigned int n=0; n<bufsize; n++)
					{
						float mix = get_child(2)->get_output()[n];
						if (mix < -1) mix = -1;
						else if (mix > 1) mix = 1;
						mix = (0.5 + (mix * 0.5));

						m_output[n]=(v0 * (1 - mix)) + (get_child(1)->get_output()[n] * mix);
					}
				}
			}
		}
		else
		{
			if (get_child(1)->is_terminal())
			{
				if (get_child(2)->is_terminal())
				{
					float v1 = get_child(1)->get_value();
					float mix = get_child(2)->get_value();
					if (mix < -1) mix = -1;
					else if (mix > 1) mix = 1;
					mix = (0.5 + (mix * 0.5));

					for (unsigned int n=0; n<bufsize; n++)
					{

						m_output[n]=(get_child(0)->get_output()[n] * (1 - mix)) + (v1 * mix);
					}
				}
				else
				{
					float v1 = get_child(1)->get_value();

					for (unsigned int n=0; n<bufsize; n++)
					{
						float mix = get_child(2)->get_output()[n];
						if (mix < -1) mix = -1;
						else if (mix > 1) mix = 1;
						mix = (0.5 + (mix * 0.5));
						m_output[n]=(get_child(0)->get_output()[n] * (1 - mix)) + (v1 * mix);
					}
				}
			}
			else
			{
				if (get_child(2)->is_terminal())
				{
					float mix = get_child(2)->get_value();
					if (mix < -1) mix = -1;
					else if (mix > 1) mix = 1;
					mix = (0.5 + (mix * 0.5));
					for (unsigned int n=0; n<bufsize; n++)
					{
						m_output[n]=(get_child(0)->get_output()[n] * (1 - mix)) + (get_child(1)->get_output()[n] * mix);
					}
				}
				else
				{
					for (unsigned int n=0; n<bufsize; n++)
					{
						float mix = get_child(2)->get_output()[n];
						if (mix < -1) mix = -1;
						else if (mix > 1) mix = 1;
						mix = (0.5 + (mix * 0.5));
						m_output[n]=(get_child(0)->get_output()[n] * (1 - mix)) + (get_child(1)->get_output()[n] * mix);
					}
				}
			}
		}
	}

}

hold_node::hold_node(type t):
graph_node(2),
m_type(t),
m_held_value(0),
m_last_ctrl_val(0)
{
}

void hold_node::trigger(float time)
{
	trigger_children(time);
}

void hold_node::process(unsigned int bufsize)
{
	if (bufsize>(unsigned int)m_output.get_length())
	{
		m_output.allocate(bufsize);
	}
	process_children(bufsize);

	if (child_exists(0) && child_exists(1))
	{
		if (get_child(0)->is_terminal() && get_child(1)->is_terminal())
		{
			if (get_child(1)->get_value() > 0) m_held_value=get_child(0)->get_value();
			for (unsigned int n=0; n<bufsize; n++) m_output[n]=m_held_value;
		}
		else if (!get_child(0)->is_terminal() && get_child(1)->is_terminal())
		{
			if (get_child(1)->get_value() <= 0)
			{
				for (unsigned int n=0; n<bufsize; n++) m_output[n]=m_held_value;
			}
			else
			{
				switch (m_type)
				{
					case SAMP:
					{
						for (unsigned int n=0; n<bufsize; n++)
						{
							if (m_last_ctrl_val <= 0)	m_held_value = get_child(0)->get_output()[n];
							m_last_ctrl_val = get_child(1)->get_value();
							m_output[n]=m_held_value;
						}
						break;
					}
					case TRACK:
					{
						for (unsigned int n=0; n<bufsize; n++)
						{
							m_output[n] = get_child(0)->get_output()[n];
						}
						break;
					}
				}
			}
		}
		else if (get_child(0)->is_terminal() && !get_child(1)->is_terminal())
		{
			for (unsigned int n=0; n<bufsize; n++)
			{
				if  (get_child(1)->get_output()[n] > 0)
				{
					m_held_value = get_child(0)->get_value();
				}
				m_output[n] = m_held_value;
			}
		}
		else
		{
			switch (m_type)
			{
				case SAMP:
				{
					for (unsigned int n=0; n<bufsize; n++)
					{
						if (m_last_ctrl_val <= 0 && get_child(1)->get_output()[n] > 0) m_held_value = get_child(0)->get_output()[n];
						m_last_ctrl_val = get_child(1)->get_output()[n];
						m_output[n] = m_held_value;
					}
					break;
				}
				case TRACK:
				{
					for (unsigned int n=0; n<bufsize; n++)
					{
						if (get_child(1)->get_output()[n] > 0) m_held_value = get_child(0)->get_output()[n];
						m_output[n] = m_held_value;
					}
					break;
				}
			}
		}
	}
}

pad_node::pad_node(unsigned int sample_rate):
    graph_node(4),
    m_pad(sample_rate)
{
}

void pad_node::trigger(float time)
{
	trigger_children(time);

	float freq=440;
	if (child_exists(0) && get_child(0)->is_terminal())
	{
		freq=get_child(0)->get_value();
	}

	if (child_exists(1) && get_child(1)->is_terminal())
	{
		m_pad.set_gap(get_child(1)->get_value());
	}

	if (child_exists(2) && get_child(2)->is_terminal())
	{
		m_pad.set_cutoff(get_child(2)->get_value());
	}

	if (child_exists(3) && get_child(3)->is_terminal())
	{
		m_pad.set_resonance(get_child(3)->get_value());
	}

	m_pad.trigger(time, freq, freq, 1);
}

void pad_node::process(unsigned int bufsize)
{
	if (bufsize>(unsigned int)m_output.get_length())
	{
		m_output.allocate(bufsize);
	}
	process_children(bufsize);

    bool have_freqCV = false;
    bool have_gapCV = false;

    // if frequency cv exists
	have_freqCV=child_exists(0) && !get_child(0)->is_terminal();
	have_gapCV=child_exists(1) && !get_child(1)->is_terminal();

/*    if (have_freqCV)
    {
        if (have_gapCV)
        {
            m_pad.process(bufsize, m_output,
                          get_child(0)->get_output(),
                          get_child(1)->get_output());
        }
        else
        {
            m_pad.processFM(bufsize, m_output, get_child(0)->get_output());
        }
    }
    else
    {
        if (have_gapCV)
        {
            m_pad.process(bufsize, m_output, get_child(1)->get_output());
        }
        else
        {*/
            m_pad.process(bufsize, m_output);
            // }
            //}
}
