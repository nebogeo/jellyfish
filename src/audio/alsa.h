#include <stdio.h>
#include <stdlib.h>
#include <alsa/asoundlib.h>
#include <limits.h>

#include <fluxa/Graph.h>

class alsa_device {
public:
  alsa_device();
  ~alsa_device();

  void start_crank(Graph *g);

  void play(size_t length, short* buf);
  Graph *m_graph;
 private:
  snd_pcm_t *playback_handle;

};
