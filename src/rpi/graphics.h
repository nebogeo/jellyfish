#include "engine/importgl.h"

typedef struct
{
   uint32_t screen_width;
   uint32_t screen_height;
// OpenGL|ES objects
   EGLDisplay display;
   EGLSurface surface;
   EGLContext context;
} RPI_STATE_T;

static volatile int terminate_prog;
static RPI_STATE_T _state, *state=&_state;

static void init_ogl_rpi(RPI_STATE_T *state);
