#include "engine/importgl.h"

#ifdef FLX_RPI

#ifndef RPI_GRAPHICS
#define RPI_GRAPHICS

typedef struct
{
   uint32_t screen_width;
   uint32_t screen_height;
// OpenGL|ES objects
   EGLDisplay display;
   EGLSurface surface;
   EGLContext context;
} RPI_STATE_T;


void init_ogl_rpi(RPI_STATE_T *state);

#endif

#endif
