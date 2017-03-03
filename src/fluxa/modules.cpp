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

#include "modules.h"
#include <stdlib.h>
#include <math.h>
#include <iostream>

using namespace std;

static float small_number = (1.0 / 4294967295.0);   // very small amount (denormal fix)

float rand_range(float L, float H) {
  return ((rand()%10000/10000.0f)*(H-L))+L;
}

void crush(sample &buf, float freq, float bits) {
  float step = pow((float)0.5,(float)bits);
  float phasor = 1;
  float last = 0;

  for(unsigned int i=0; i<buf.get_length(); i++) {
      phasor = phasor + freq;
      if (phasor >= 1.0) {
          phasor = phasor - 1.0;
          last = step * floor( buf[i]/step + 0.5 );
	}
      buf[i] = last;
    }
}

void distort(sample &buf, float amount) {
  if (amount>=0.99) amount = 0.99;

  float k=2*amount/(1-amount);

  for(unsigned int i=0; i<buf.get_length(); i++)
    {
      buf[i]=((1+k)*buf[i]/(1+k*fabs(buf[i])))*(1-amount);
    }
}


void moving_distort(sample &buf, const sample &amount)
{
  for(unsigned int i=0; i<buf.get_length(); i++)
    {
      float a =fabs(amount[i]);
      if (a>0.99) a = 0.99;
      float k=2*a/(1-a);

      buf[i]=((1+k)*buf[i]/(1+k*fabs(buf[i])))*(1-a);
    }
}

void hard_clip(sample &buf, float level)
{
  if (feq(level,0,0.0001)) level==0.0001;

  for(unsigned int i=0; i<buf.get_length(); i++)
    {
      if (buf[i]>level) buf[i]=level;
      if (buf[i]<-level) buf[i]=-level;
      buf[i]*=1/level;
    }
}

void moving_hard_clip(sample &buf, const sample &level)
{
  for(unsigned int i=0; i<buf.get_length(); i++)
    {
      float l=fabs(level[i]);
      if (feq(l,0,0.0001)) l=0.0001;
      if (buf[i]>l) buf[i]=l;
      if (buf[i]<-l) buf[i]=-l;
      buf[i]*=1/l;
    }
}

///////////////////////////////////////////////////////////////////////////

unsigned int wave_table::m_table_length=DEFAULT_TABLE_LEN;
sample wave_table::m_table[NUM_TABLES];

wave_table::wave_table(int sample_rate) :
  module(sample_rate)
{
  m_time_per_sample=1/(float)m_sample_rate;
  m_cycle_pos=0;
  m_pitch=m_sample_rate/DEFAULT_TABLE_LEN;
  m_target_pitch=m_pitch;
  m_volume=1.0f;
  m_slide_time=0;
  reset();
  m_table_per_sample=m_table_length/(float)sample_rate;
}

void wave_table::reset()
{
  m_type=SINE;
  m_octave=0;
  m_fine_freq=1.0f;
  m_slide_time=0;
  m_slide_length=0;
}

void wave_table::write_waves()
{
  cerr<<"writing waves"<<endl;
  for (int n=0; n<NUM_TABLES; n++)
    {
      m_table[n].allocate(m_table_length);
    }

  float rad_cycle = (M_PI/180)*360;
  float pos=0;

  for (unsigned int n=0; n<m_table_length; n++)
    {
      if (n==0) pos=0;
      else pos=(n/(float)m_table_length)*rad_cycle;
      m_table[NOISE].set(n,rand_range(-1,1));
    }

  // todo - might be better to run this a few cycles before storing
  float white=0;
  float b0=0,b1=0,b2=0,b3=0,b4=0,b5=0,b6=0;
  for (unsigned int n=0; n<m_table_length; n++)
    {
      white=(1.0f-((rand()%INT_MAX)/(float)INT_MAX)*2.0)*0.2f;
      b0 = 0.99886f * b0 + white * 0.0555179f;
      b1 = 0.99332f * b1 + white * 0.0750759f;
      b2 = 0.96900f * b2 + white * 0.1538520f;
      b3 = 0.86650f * b3 + white * 0.3104856f;
      b4 = 0.55000f * b4 + white * 0.5329522f;
      b5 = -0.7616f * b5 - white * 0.0168980f;
      m_table[PINKNOISE].set(n,b0 + b1 + b2 + b3 + b4 + b5 + b6 + white * 0.5362f);
      b6 = white * 0.115926f;
    }

  for (unsigned int n=0; n<m_table_length; n++)
    {
      if (n==0) pos=0;
      else pos=(n/(float)m_table_length)*rad_cycle;
      m_table[SINE].set(n,sin(pos));
    }

  for (unsigned int n=0; n<m_table_length; n++)
    {
      if (n<m_table_length/2) m_table[SQUARE].set(n,1.0f);
      else m_table[SQUARE].set(n,-1);
    }

  for (unsigned int n=0; n<m_table_length; n++)
    {
      m_table[REVSAW].set(n,((n/(float)m_table_length)*2.0f)-1.0f);
    }

  for (unsigned int n=0; n<m_table_length; n++)
    {
      m_table[SAW].set(n,1-(n/(float)m_table_length)*2.0f);
    }

  float half_tab=m_table_length/2;
  float v=0;
  for (unsigned int n=0; n<m_table_length; n++)
    {
      if (n<half_tab) v=1-(n/half_tab)*2.0f;
      else v=(((n-half_tab)/half_tab)*2.0f)-1.0f;
      v*=0.99;
      m_table[TRIANGLE].set(n,v);
    }

  for (unsigned int n=0; n<m_table_length; n++)
    {
      if (n<m_table_length/1.2) m_table[PULSE1].set(n,1);
      else m_table[PULSE1].set(n,-1);
    }

  for (unsigned int n=0; n<m_table_length; n++)
    {
      if (n<m_table_length/1.5) m_table[PULSE2].set(n,1);
      else m_table[PULSE2].set(n,-1);
    }
  cerr<<"written waves"<<endl;

}

void wave_table::trigger(float time, float pitch, float slidepitch, float vol)
{
  m_target_pitch=pitch;
  if (m_slide_length==0) m_pitch=pitch;
  m_volume=vol*1.0f;
  m_slide_time=0;
}

void wave_table::process(unsigned int buf_size, sample &in)
{
  if (m_slide_length>0)
    {
      float incr;
      float freq;
      float start_freq=m_pitch;
      start_freq*=m_fine_freq;
      if (m_octave>0) start_freq*=1<<(m_octave);
      if (m_octave<0) start_freq/=1<<(-m_octave);

      float slide_freq=m_target_pitch;
      slide_freq*=m_fine_freq;
      if (m_octave>0) slide_freq*=1<<(m_octave);
      if (m_octave<0) slide_freq/=1<<(-m_octave);

      for (unsigned int n=0; n<buf_size; n++)
	{
	  float t=m_slide_time/m_slide_length;
	  if (t>1) freq=slide_freq;
	  else freq=(1-t)*start_freq+t*slide_freq;
	  incr = freq*m_table_per_sample;
	  m_cycle_pos+=incr;
	  if (m_cycle_pos<0) m_cycle_pos=m_table_length-m_cycle_pos;
	  m_cycle_pos=fmod(m_cycle_pos,m_table_length-1);
	  in[n]=m_table[(int)m_type][m_cycle_pos]*m_volume;
	  m_slide_time+=m_time_per_sample;
	}
    }
  else
    {
      float incr;
      float freq=m_pitch;
      freq*=m_fine_freq;
      if (m_octave>0) freq*=1<<(m_octave);
      if (m_octave<0) freq/=1<<(-m_octave);
      incr = freq*m_table_per_sample;

      for (unsigned int n=0; n<buf_size; n++)
	{
	  m_cycle_pos+=incr;
	  if (m_cycle_pos<0) m_cycle_pos=m_table_length-m_cycle_pos;
	  m_cycle_pos=fmod(m_cycle_pos,m_table_length-1);
	  in[n]=m_table[(int)m_type][m_cycle_pos]*m_volume;
	}
    }
}

void wave_table::processFM(unsigned int buf_size, sample &in, const sample &pitch)
{
  for (unsigned int n=0; n<buf_size; n++)
    {
      if (isfinite(pitch[n]))
	{
	  m_cycle_pos+=pitch[n]*m_table_per_sample;
	  if (m_cycle_pos<0) m_cycle_pos=m_table_length-m_cycle_pos;
	  m_cycle_pos=fmod(m_cycle_pos,m_table_length-1);
	  in[n]=m_table[(int)m_type][m_cycle_pos]*m_volume;
	}
    }
}

void wave_table::simple_process(unsigned int buf_size, sample &in)
{
  float incr = m_pitch*m_fine_freq*(m_table_length/(float)m_sample_rate);
  for (unsigned int n=0; n<buf_size; n++)
    {
      m_cycle_pos+=incr;
      if (m_cycle_pos<0) m_cycle_pos=m_table_length-m_cycle_pos;
      m_cycle_pos=fmod(m_cycle_pos,m_table_length-1);
      in[n]+=m_table[(int)m_type][m_cycle_pos]*m_volume;
    }
}

///////////////////////////////////////////////////////////////////////////

simple_wave::simple_wave(int sample_rate) :
  module(sample_rate),
  m_table_length(DEFAULT_TABLE_LEN)
{
  m_cycle_pos=0;
  reset();

  for (int n=0; n<NUM_TABLES; n++)
    {
      m_table.allocate(m_table_length);
    }

  write_waves();
}

void simple_wave::reset()
{
  m_pitch=0;
  m_fine_freq=1.0f;
}

void simple_wave::write_waves()
{
  float rad_cycle = (M_PI/180)*360;
  float pos=0;

  for (unsigned int n=0; n<m_table_length; n++)
    {
      if (n==0) pos=0;
      else pos=(n/(float)m_table_length)*rad_cycle;
      m_table.set(n,sin(pos));
    }
}

void simple_wave::trigger(float time, float pitch, float slidepitch, float vol) {
  m_pitch=pitch;
  m_slide_pitch=slidepitch;
  m_volume=vol*1.0;
}

void simple_wave::process(unsigned int buf_size, sample &in) {
  float incr = m_pitch*m_fine_freq*(m_table_length/(float)m_sample_rate);
  for (unsigned int n=0; n<buf_size; n++)
    {
      m_cycle_pos+=incr;
      if (m_cycle_pos<0) m_cycle_pos=m_table_length-m_cycle_pos;
      m_cycle_pos=fmod(m_cycle_pos,m_table_length-1);
      in[n]+=m_table[m_cycle_pos]*m_volume;
    }
}

///////////////////////////////////////////////////////////////////////////

envelope::envelope(int sample_rate) :
  module(sample_rate)
{
  m_t=-1000.0f;
  m_trigger=false;
  m_sample_time=1.0/(float)m_sample_rate;
  m_current=0;
  reset();
}

void envelope::reset()
{
  m_attack=0.0f;
  m_decay=0.2f;
  m_sustain=0.0f;
  m_release=5.0f;
  m_volume=1.0f;
  m_t=-1000.0f;
  m_trigger=false;
  m_current=0;
}

void envelope::process(unsigned int buf_size, sample &CV, bool smooth)
{
  if (m_attack==0 && m_decay==0 && m_release==0)
    {
      return;
    }

  smooth=true;

  // a bit of a crap filter to smooth clicks
  static float SMOOTH = 0.98;
  static float ONEMINUS_SMOOTH = 1-SMOOTH;

  float temp=0;
  bool freeze=false;
  float nt;

  if (m_t==-1000)
    {
      CV.zero();
      m_current=0;
      return;
    }


  for (unsigned int n=0; n<buf_size; n++)
    {

      // if we are in the delay (before really being triggered)
      if (m_t<0)
	{
	  float temp=0;
	  if (!feq(temp,m_current,0.01) && smooth)
	    {
	      // only filter if necc
	      temp=(temp*ONEMINUS_SMOOTH+m_current*SMOOTH);
	    }
	  CV[n]=temp;
	  m_current=temp;
	  m_t+=m_sample_time;
	}
      else // in the envelope
	{
	  // if we are in the envelope...
	  if (m_t>=0 && m_t<m_attack+m_decay+m_release)
	    {
	      // find out what part of the envelope we are in
	      // in the attack
	      if (m_t<m_attack)
		{
		  // get normalised position to
		  // get the volume between 0 and 1
		  temp=m_t/m_attack;
		}
	      else
		// in the decay
		if (m_t<m_attack+m_decay)
		  {
		    // normalised position in m_attack->m_decay range
		    nt=(m_t-m_attack)/m_decay;

		    // volume between 1 and m_sustain
		    temp=(1-nt)+(m_sustain*nt);
		  }
		else // in the release
		  {
		    // normalised position in m_decay->m_release range
		    nt=(m_t-(m_attack+m_decay))/m_release;

		    // volume between m_sustain and 0
		    temp=m_sustain*(1-nt);

		    if (m_release<0.2f)
		      {
			temp=m_sustain;
		      }

		    //if (m_trigger) freeze=true;
		  }

	      temp*=m_volume;

	      if (!feq(temp,m_current,0.01) && smooth)
		{
		  // only filter if necc
		  temp=(temp*ONEMINUS_SMOOTH+m_current*SMOOTH);
		}
	      CV[n]=temp;
	      m_current=temp;

	      if (!freeze) m_t+=m_sample_time;
	    }
	  else
	    {
	      if (!feq(temp,m_current,0.01) && smooth)
		{
		  temp=m_current*SMOOTH;
		}

	      CV[n]=temp;
	      m_current=temp;

	      // if we've run off the end
	      if (m_t>m_attack+m_decay+m_release)
		{
		  m_t=-1000;
		}
	    }
	}
    }
}

void envelope::trigger(float time, float pitch, float vol)
{
  if (vol<0.0001)
    {
      if (m_t!=-1000 && m_t<m_attack+m_decay+m_release) m_t=m_attack+m_decay+m_release;
    }
  else
    {
      m_t=time;
    }
}

///////////////////////////////////////////////////////////////////////////

simple_envelope::simple_envelope(int sample_rate) :
  module(sample_rate)
{
  m_trigger=false;
  m_t=-1.0f;
  m_current=0;
  m_sample_time=1.0/(float)m_sample_rate;
  reset();
  m_decay=1.0f;
  m_volume=1.0f;
}

void simple_envelope::reset()
{
  //m_decay=1.0f;
  //m_volume=0.1f;
}

void simple_envelope::process(unsigned int buf_size, sample &in, sample &CV, bool smooth)
{
  // a bit of a crap filter to smooth clicks
  static float SMOOTH = 0.999;
  static float ONEMINUS_SMOOTH = 1-SMOOTH;
  float one_over_decay=1/m_decay;
  float temp=0;

  if (m_t==-1000)
    {
      in.zero();
      CV.zero();
      m_current=0;
      return;
    }

  for (unsigned int n=0; n<buf_size; n++)
    {
      // if we are in the delay (before really being triggered)
      if (m_t<0)
	{
	  in[n]*=m_current;
	  CV[n]=m_current;
	}
      else // in the envelope
	{
	  // if we are in the envelope...
	  if (m_t<m_decay)
	    {
	      // in the decay
	      temp=(1-m_t*one_over_decay)*m_volume;
	      if (!feq(temp,m_current,0.01) && smooth)
		{
		  // only filter if necc
		  temp=(temp*ONEMINUS_SMOOTH+m_current*SMOOTH);
		}
	      in[n]*=temp;
	      CV[n]=temp;
	      m_current=temp;
	    }
	  else
	    {
	      in[n]*=0;
	      CV[n]=0;
	      m_current=0;

	      // we've run off the end
	      m_t=-1000;
	    }
	}

      m_t+=m_sample_time;
    }
}

void simple_envelope::trigger(float time, float pitch, float vol)
{
  m_t=time;
}

///////////////////////////////////////////////////////////////////////////
// Csound source code, stilson/smith CCRMA paper., paul kellett version
// moog VCF, variation 1 from musicdsp archive

moog_filter::moog_filter(int sample_rate) :
  module(sample_rate),
  fs(sample_rate),
  fc(1000.0f),
  f(0.0f),
  p(0.0f),
  q(0.0f),
  b0(0.1f),
  b1(0.1f),
  b2(0.0f),
  b3(0.0f),
  b4(0.0f),
  t1(0.0f),
  t2(0.0f)
{
  reset();
}

void moog_filter::reset()
{
  cutoff=0.5f;
  resonance=0.0f;
}

void moog_filter::process(unsigned int buf_size, sample &sin, sample *cutoffCV, sample *LPFout, sample *BPFout, sample *HPFout)
{
  float in=0,Q=0;
  for (unsigned int n=0; n<buf_size; n++)
    {
      if (n%FILTER_GRANULARITY==0)
	{
	  fc = cutoff;
	  if (cutoffCV!=NULL) fc+=(*cutoffCV)[n];
	  fc*=0.25;
	  if (fc<0) fc=0;
	  else if (fc>1) fc=1;

	  q = 1.0f - fc;
	  p = fc + 0.8f * fc * q;
	  f = p + p - 1.0f;
	  Q = resonance*6-3;
	  q = Q + (1.0f + 0.5f * q * (1.0f - q + 5.6f * q * q));
	}

      in = sin[n];

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

      if (LPFout) (*LPFout)[n]=b4;
      if (BPFout) (*BPFout)[n]=(in-b4);
      if (HPFout) (*HPFout)[n]=3.0f * (b3 - b4);
    }
}

///////////////////////////////////////////////////////////////////////////

//-------------------------------------------------------------VOWEL COEFFICIENTS
const double coeff[5][11]= {
  { 8.11044e-06,
    8.943665402, -36.83889529, 92.01697887, -154.337906, 181.6233289,
    -151.8651235,   89.09614114, -35.10298511, 8.388101016, -0.923313471  ///A
  },
  {4.36215e-06,
   8.90438318, -36.55179099, 91.05750846, -152.422234, 179.1170248,  ///E
   -149.6496211,87.78352223, -34.60687431, 8.282228154, -0.914150747
  },
  { 3.33819e-06,
    8.893102966, -36.49532826, 90.96543286, -152.4545478, 179.4835618,
    -150.315433, 88.43409371, -34.98612086, 8.407803364, -0.932568035  ///I
  },
  {1.13572e-06,
   8.994734087, -37.2084849, 93.22900521, -156.6929844, 184.596544,   ///O
   -154.3755513, 90.49663749, -35.58964535, 8.478996281, -0.929252233
  },
  {4.09431e-07,
   8.997322763, -37.20218544, 93.11385476, -156.2530937, 183.7080141,  ///U
   -153.2631681, 89.59539726, -35.12454591, 8.338655623, -0.910251753
  }
};

formant_filter::formant_filter(int sample_rate) :
  module(sample_rate)
{
  reset();
}

void formant_filter::reset()
{
  for (int x=0; x<5; x++)
    for (int y=0; y<10; y++)
      memory[x][y]=0;
  m_vowel=0;
}

void formant_filter::process(unsigned int buf_size, sample &sin, sample *cutoffCV, sample &sout)
{
  float res,o[5],out=0, in=0;

  for (unsigned int n=0; n<buf_size; n++)
    {
      in = sin[n];

      // work around denormal calculation CPU spikes where in --> 0
      if ((in >= 0) && (in < 0.000000001))
	in += 0.000000001;
      else
	if ((in <= 0) && (in > -0.000000001))
	  in -= 0.000000001;

      for (int v=0; v<5; v++)
	{
	  res= (float) (coeff[v][0]*in +
			coeff[v][1]*memory[v][0] +
			coeff[v][2]*memory[v][1] +
			coeff[v][3]*memory[v][2] +
			coeff[v][4]*memory[v][3] +
			coeff[v][5]*memory[v][4] +
			coeff[v][6]*memory[v][5] +
			coeff[v][7]*memory[v][6] +
			coeff[v][8]*memory[v][7] +
			coeff[v][9]*memory[v][8] +
			coeff[v][10]*memory[v][9]);

	  memory[v][9]=memory[v][8];
	  memory[v][8]=memory[v][7];
	  memory[v][7]=memory[v][6];
	  memory[v][6]=memory[v][5];
	  memory[v][5]=memory[v][4];
	  memory[v][4]=memory[v][3];
	  memory[v][3]=memory[v][2];
	  memory[v][2]=memory[v][1];
	  memory[v][1]=memory[v][0];
	  memory[v][0]=(double) res;

	  o[v]=res;
	}

      float vowel=m_vowel;
      if (cutoffCV!=NULL) vowel+=(*cutoffCV)[n];

      // mix between vowel sounds
      if (vowel<1)
	{
	  out=linear(0,1,vowel,o[1],o[0]);
	}
      else
	if (vowel>1 && vowel<2)
	  {
	    out=linear(0,1,vowel-1.0f,o[2],o[1]);
	  }
	else
	  if (vowel>2 && vowel<3)
	    {
	      out=linear(0,1,m_vowel-2.0f,o[3],o[2]);
	    }
	  else
	    if (vowel>3 && vowel<4)
	      {
		out=linear(0,1,vowel-3.0f,o[4],o[3]);
	      }
	    else
	      if (vowel==4)
		{
		  out=o[4];
		}

      sout[n]=out;
    }
}

///////////////////////////////////////////////////////////////////////////

filter_wrapper::filter_wrapper(int sample_rate):
  module(sample_rate),
  m_moog_filter(sample_rate),
  m_formant_filter(sample_rate)
{
}

void filter_wrapper::reset()
{
  m_moog_filter.reset();
  m_formant_filter.reset();
}

void filter_wrapper::process(unsigned int buf_size, sample &in, sample &cutoffCV, sample &out)
{
  switch (m_type)
    {
    case MOOG_LO : m_moog_filter.process(buf_size, in, &cutoffCV, &out, NULL, NULL); break;
    case MOOG_BAND : m_moog_filter.process(buf_size, in, &cutoffCV, NULL, &out, NULL); break;
    case MOOG_HI : m_moog_filter.process(buf_size, in, &cutoffCV, NULL, NULL, &out); break;
    case FORMANT : m_formant_filter.process(buf_size, in, &cutoffCV, out); break;
    }
}

void filter_wrapper::process(unsigned int buf_size, sample &in, sample &out)
{
  switch (m_type)
    {
    case MOOG_LO : m_moog_filter.process(buf_size, in, NULL, &out, NULL, NULL); break;
    case MOOG_BAND : m_moog_filter.process(buf_size, in, NULL, NULL, &out, NULL); break;
    case MOOG_HI : m_moog_filter.process(buf_size, in, NULL, NULL, NULL, &out); break;
    case FORMANT : m_formant_filter.process(buf_size, in, NULL, out); break;
    }
}

///////////////////////////////////////////////////////////////////////////

static const float MAX_DELAYTIME=2.0;

delay::delay(int sample_rate) :
  module(sample_rate),
  m_position(0)
{
  reset();
  m_buffer.allocate((int)(MAX_DELAYTIME*m_sample_rate));
}

void delay::reset()
{
  m_delay=0;
  m_feedback=0;
}

void delay::process(unsigned int buf_size, sample &in, sample &delayCV, sample &feedbackCV, sample &out)
{

}

void delay::process(unsigned int buf_size, sample &in, sample &out)
{
  unsigned int delay=(unsigned int)(m_sample_rate*m_delay);

  if (delay==0)
    {
      return;
    }

  if (delay>=(unsigned int)m_buffer.get_length()) delay=m_buffer.get_length()-1;

  for (unsigned int n=0; n<buf_size; n++)
    {
      m_buffer[m_position]=in[n]+m_buffer[m_position]*m_feedback;
      out[n]=m_buffer[m_position];
      m_position=(m_position+1)%delay;
    }
}

///////////////////////////////////////////////////////////////////////////
// (c) neil C / etanza systems / 2K6
//
// shouts / loves / moans = etanza at lycos dot co dot uk
//
// this work is hereby placed in the public domain for all purposes, including
// use in commercial applications.
//
// the author assumes NO RESPONSIBILITY for any problems caused by the use of
// this software.
//
//----------------------------------------------------------------------------

// NOTES :
//
// - original filter code by paul kellet (musicdsp.pdf)
//
// - uses 4 first order filters in series, should give 24dB per octave
//
// - now with P4 denormal fix :)

eq::eq(int sample_rate) :
  module(sample_rate),
  lf(0),
  f1p0(0),
  f1p1(0),
  f1p2(0),
  f1p3(0),
  hf(0),
  f2p0(0),
  f2p1(0),
  f2p2(0),
  f2p3(0),
  sdm1(0),
  sdm2(0),
  sdm3(0),
  m_low(1),
  m_mid(1),
  m_high(1)
{
  lf = 2 * sin(M_PI * (880.0f / (float)sample_rate));
  hf = 2 * sin(M_PI * (5000.0f / (float)sample_rate));
}

void eq::process(unsigned int buf_size, sample &in)
{
  for (unsigned int n=0; n<buf_size; n++)
    {
      float  l,m,h; // low / mid / high - sample values

      // filter #1 (lowpass)
      f1p0  += (lf * (in[n] - f1p0));// + small_number;
      f1p1  += (lf * (f1p0 - f1p1));
      f1p2  += (lf * (f1p1 - f1p2));
      f1p3  += (lf * (f1p2 - f1p3));
      l = f1p3;

      // filter #2 (highpass)
      f2p0  += (hf * (in[n] - f2p0));// + small_number;
      f2p1  += (hf * (f2p0 - f2p1));
      f2p2  += (hf * (f2p1 - f2p2));
      f2p3  += (hf * (f2p2 - f2p3));
      h = sdm3 - f2p3;

      // calculate midrange (signal - (low + high))
      m = sdm3 - (h + l);

      // scale, combine and store
      l *= m_low;
      m *= m_mid;
      h *= m_high;

      // shuffle history buffer
      sdm3 = sdm2;
      sdm2 = sdm1;
      sdm1 = in[n];

      // return result
      in[n]=(l + m + h);
    }
}

// type : hardknee compressor with RMS look-ahead envelope calculation and adjustable attack/decay
// references : posted by flashinc[AT]mail[DOT]ru
//
// notes :
// RMS is a true way to estimate musical_ signal energy,
// our ears behaves in a same way.

compressor::compressor(int sample_rate) :
  module(sample_rate),
  threshold(0.5),
  slope(0.5),
  sr(sample_rate),
  tla(1.0f*1e-3),
  twnd(3.0f*1e-3),
  tatt(0.1f*1e-3),
  trel(300.0f*1e-3)
{
}

void compressor::process(unsigned int buf_size, sample &in)
{
  // attack and release "per sample decay"
  float att=(tatt == 0.0) ? (0.0) : exp (-1.0 / (sr * tatt));
  float rel=(trel == 0.0) ? (0.0) : exp (-1.0 / (sr * trel));
  // envelope
  float env = 0.0;
  // sample offset to lookahead wnd start
  int lhsmp = (int)(sr*tla);
  // samples count in lookahead window
  int nrms = (int)(sr*twnd);

  // for each sample...
  for (unsigned int i=0; i<buf_size; ++i)
    {
      // now compute RMS
      float summ = 0;

      // for each sample in window
      for (int j=0; j<nrms; ++j)
        {
	  unsigned int lki = i + j + lhsmp;
	  float  smp;
	  if (lki < buf_size) smp = in[lki];
	  else smp = 0.0;
	  summ += smp * smp;  // square em..
        }

      float rms = sqrt (summ / nrms);   // root-mean-square
      // dynamic selection: attack or release?
      float theta = rms > env ? att : rel;
      // smoothing with capacitor, envelope extraction...
      // here be aware of pIV denormal numbers glitch
      env = (1.0 - theta) * rms + theta * env;
      // the very easy hard knee 1:N compressor
      float  gain = 1.0;
      if (env > threshold) gain = gain - (env - threshold) * slope;
      // result - two hard kneed compressed channels...
      in[i] *= gain;
      //		if (i==0) cerr<<threshold<<" "<<env<<" "<<gain<<endl;
    }



}


///////////////////////////////////////////////////////////////////////////

KS::KS(int sample_rate) :
  module(sample_rate),
  m_position(0),
  m_filter(sample_rate)
{
  reset();
  m_buffer.allocate((int)(MAX_DELAYTIME*m_sample_rate));
}

void KS::reset()
{
  m_delay=0;
  m_feedback=0;
  m_filter.reset();
}

void KS::trigger(float time, float pitch, float slidepitch, float vol)
{
  m_delay=1.0f/pitch;
  //m_volume=vol*1.0;

  unsigned int delay=(unsigned int)(m_sample_rate*m_delay);

  for (unsigned int n=0; n<delay; n++)
    {
      m_buffer[n]=rand_range(-1,1);
    }
}

void KS::process(unsigned int buf_size, sample &out)
{
  unsigned int delay=(unsigned int)(m_sample_rate*m_delay);

  if (delay==0) return;
  if (delay>=(unsigned int)m_buffer.get_length())
    {
      delay=m_buffer.get_length()-1;
    }

  for (unsigned int n=0; n<buf_size; n++)
    {
      m_buffer[m_position]=m_filter.process_single(m_buffer[m_position]);
      out[n]=m_buffer[m_position];
      m_position=(m_position+1)%delay;
    }
}

//////////////////////////////////////////////////////////////////////////////////////

pad::pad(int sample_rate) :
  module(sample_rate),
  m_table_length(4096),
  m_filter(sample_rate)
{
  reset();

  m_table_per_sample=m_table_length/(float)sample_rate;
  m_cycle_pos=0;
  m_write_pos=0;
  m_state=0;
  m_table.allocate(m_table_length);

  m_filter.set_cutoff(0.1);
  m_filter.set_resonance(0.3);

  write_waves();
}

void pad::reset()
{
  m_filter.reset();
  m_pitch=0;
}

void pad::write_waves()
{
  float rad_cycle = (M_PI/180)*360;
  float pos=0;
  //    m_table.zero();
  m_table.set(0,1);
}

void pad::trigger(float time, float pitch, float slidepitch, float vol)
{
  m_pitch=pitch;
  m_volume=vol*1.0;
  //    write_waves();
  m_cycle_pos=0;
  m_write_pos=0;
}

void pad::process(unsigned int buf_size, sample &in)
{
  float incr = m_pitch*(m_table_length/(float)m_sample_rate);

  if (m_gap==0) m_gap=0.00001;
  if (m_gap>1) m_gap=1;

  for (unsigned int n=0; n<buf_size; n++)
    {
      m_cycle_pos+=incr;
      m_write_pos++;

      in[n]=0;
      for (int i=0; i<12; i++)
        {
	  in[n]+=m_table[m_cycle_pos*((i+1)*m_gap)]/12.0f;
        }

      if (m_table[m_write_pos]<-1 || m_table[m_write_pos]>1)
	m_table[m_write_pos]=m_filter.process_single(in[n]+m_table[m_write_pos])*0.99;
      else
	m_table[m_write_pos]=m_filter.process_single(in[n]+m_table[m_write_pos]);


    }
}
