#include "alsa.h"
#include <pthread.h>
#include <iostream>

using namespace std;

alsa_device::alsa_device() {
  int i;
  int err;
  short buf[128];
  snd_pcm_hw_params_t *hw_params;

  if ((err = snd_pcm_open(&playback_handle, "hw:0,0", SND_PCM_STREAM_PLAYBACK, 0)) < 0) {
      printf("Playback open error: %s\n", snd_strerror(err));
      exit(EXIT_FAILURE);
  }
  if ((err = snd_pcm_set_params(playback_handle,
                                SND_PCM_FORMAT_S16_LE,
                                SND_PCM_ACCESS_RW_INTERLEAVED,
                                2,
                                48000,
                                1,
                                500000)) < 0) { /* 0.5sec */
      printf("Playback open error: %s\n", snd_strerror(err));
      exit(EXIT_FAILURE);      
  }
  cerr<<"started..."<<endl;
}

alsa_device::~alsa_device() {
  snd_pcm_close (playback_handle);
}

#define AUDIO_BUFSIZE 4096


void audio_loop(void *c) {

  alsa_device *a=(alsa_device *)c;
  Graph *g = a->m_graph;
  short *data = new short(AUDIO_BUFSIZE*2);
  
  Sample left(AUDIO_BUFSIZE);
  Sample right(AUDIO_BUFSIZE);
  
  do {
    left.Zero();
    right.Zero();
    
    g->Process(AUDIO_BUFSIZE, left, right);
    
    unsigned int pos=0;
    for (unsigned int i=0; i<AUDIO_BUFSIZE*2; i+=2)
      {
	data[i]=(short)(left[pos++]*3276);
      }
	
    pos=0;
    for (unsigned int i=1; i<AUDIO_BUFSIZE*2; i+=2)
      {
	data[i]=(short)(right[pos++]*3276);
      }
    
    a->play(AUDIO_BUFSIZE, data);
  } while(1);
}


void alsa_device::start_crank(Graph *g)
{
    m_graph=g;
    pthread_t *audio_thread = new pthread_t;
    pthread_create(audio_thread,NULL,(void*(*)(void*))audio_loop,this);
}


void alsa_device::play(size_t length, short* buf) {
  int frames;
  frames = snd_pcm_writei(playback_handle, buf, length);
  if (frames < 0)
      frames = snd_pcm_recover(playback_handle, frames, 0);
  if (frames < 0) {
      printf("snd_pcm_writei failed: %s\n", snd_strerror(frames));
   }
  if (frames > 0 && frames < (long)length)
      printf("Short write (expected %li, wrote %li)\n", (long)length, frames);
}
