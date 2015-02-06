#include <stdio.h>
#include <stdlib.h>
#include <alsa/asoundlib.h>
#include <limits.h>

class alsa_device {  
  alsa_device();
  ~alsa_device();

  void play(short* buf, size_t length);

 private:
  snd_pcm_t *playback_handle;
 
};
