#include "alsa.h"
      
alsa_device::alsa_device() {
  int i;
  int err;
  short buf[128];
  snd_pcm_hw_params_t *hw_params;
  
  if ((err = snd_pcm_open (&playback_handle, "hw:0,0", SND_PCM_STREAM_PLAYBACK, 0)) < 0) {
    fprintf (stderr, "cannot open audio device %s (%s)\n", 
	     "hw:0,0",
	     snd_strerror (err));
    exit (1);
  }
     
  if ((err = snd_pcm_hw_params_malloc (&hw_params)) < 0) {
    fprintf (stderr, "cannot allocate hardware parameter structure (%s)\n",
	     snd_strerror (err));
    exit (1);
  }
   
  if ((err = snd_pcm_hw_params_any (playback_handle, hw_params)) < 0) {
    fprintf (stderr, "cannot initialize hardware parameter structure (%s)\n",
	     snd_strerror (err));
    exit (1);
  }
  
  if ((err = snd_pcm_hw_params_set_access (playback_handle, hw_params, SND_PCM_ACCESS_RW_INTERLEAVED)) < 0) {
    fprintf (stderr, "cannot set access type (%s)\n",
	     snd_strerror (err));
    exit (1);
  }
  
    if ((err = snd_pcm_hw_params_set_format (playback_handle, hw_params, SND_PCM_FORMAT_S16_LE)) < 0) {
     fprintf (stderr, "cannot set sample format (%s)\n",
	     snd_strerror (err));
    exit (1);
    }

  /*  
  if ((err = snd_pcm_hw_params_set_rate_near (playback_handle, hw_params, 48000, 0)) < 0) {
    fprintf (stderr, "cannot set sample rate (%s)\n",
	     snd_strerror (err));
    exit (1);
  }
  */
    
    if ((err = snd_pcm_hw_params_set_channels (playback_handle, hw_params, 2)) < 0) {
      fprintf (stderr, "cannot set channel count (%s)\n",
	       snd_strerror (err));
      exit (1);
    }
    
    if ((err = snd_pcm_hw_params (playback_handle, hw_params)) < 0) {
      fprintf (stderr, "cannot set parameters (%s)\n",
	       snd_strerror (err));
      exit (1);
    }
    
    snd_pcm_hw_params_free (hw_params);
    
    if ((err = snd_pcm_prepare (playback_handle)) < 0) {
      fprintf (stderr, "cannot prepare audio interface for use (%s)\n",
	       snd_strerror (err));
      exit (1);
    }
}

alsa_device::~alsa_device() {
  snd_pcm_close (playback_handle);
}
    
void alsa_device::play(short* buf, size_t length) {    
  int err;

  if ((err = snd_pcm_writei (playback_handle, buf, length)) != length) {
    fprintf (stderr, "write to audio interface failed (%s)\n",
	     snd_strerror (err));
    exit (1);    
  }
}


