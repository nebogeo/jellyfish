// Copyright (C) 2005 Dave Griffiths
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <sys/time.h>
#include <iostream>
#include <string>
#include <png.h>
#include <pthread.h>

#include <lo/lo.h>

#include "engine/importgl.h"
#include "core/fixed.h"
#include "app.h"

#ifdef FLX_RPI
#include <assert.h>
#include "bcm_host.h"

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

#endif

using namespace std;

int w,h=0;
int gAppAlive = 1;
int modifiers = 0;
pthread_mutex_t* render_mutex;

// setup the assets location
// on linux, mimic android, otherwise load locally on rpi
#ifdef ANDROID_NDK
static const string ASSETS_LOCATION("../assets/");
#else
static const string ASSETS_LOCATION("assets/");
#endif

unsigned char* LoadPNG(const string filename,long &width, long &height)
{
	unsigned char *data = NULL;
	FILE *fp=fopen(filename.c_str(),"rb");
	if (!fp || filename=="")
	{
		cerr<<"Couldn't open image ["<<filename<<"]"<<endl;
	}
	else
	{
		png_structp png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
		png_infop info_ptr = png_create_info_struct(png_ptr);

		if (setjmp(png_jmpbuf(png_ptr)))
		{
			png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
			cerr<<"Error reading image ["<<filename<<"]"<<endl;
			fclose(fp);
			return NULL;
		}

		png_init_io(png_ptr, fp);
		png_read_info(png_ptr, info_ptr);

		width = png_get_image_width(png_ptr, info_ptr);
		height = png_get_image_height(png_ptr, info_ptr);
		int bit_depth = png_get_bit_depth(png_ptr, info_ptr);
		int colour_type = png_get_color_type(png_ptr, info_ptr);
		png_bytep *row_pointers=new png_bytep[height];
		unsigned int rb = png_get_rowbytes(png_ptr, info_ptr);

		for (unsigned long row=0; row<height; row++)
		{
			row_pointers[row] = new png_byte[rb];
		}

		// read the data into the row pointers
		png_read_image(png_ptr, row_pointers);
		fclose(fp);

		// make a new contiguous array to store the pixels
		data=new unsigned char[rb*height];
		int p=0;
		for (int row = 0; row<height; row++)
		{
			for (unsigned int i=0; i<rb; i++)
			{
				data[p]=(unsigned char)(row_pointers[row])[i];
				p++;
			}
		}

		// clear up the row_pointers
		for (unsigned long row=0; row<height; row++)
		{
			delete[] row_pointers[row];
		}
		delete[] row_pointers;

		png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp)NULL);
	}
    return data;
}


string LoadFile(string filename)
{
    FILE *file=fopen(filename.c_str(),"r");
	if (file)
	{
		fseek(file,0,SEEK_END);
		long size=ftell(file);
		fseek(file,0,SEEK_SET);

		char *buffer = new char[size+1];
        long s = (long)fread(buffer,1,size,file);
        buffer[s]='\0';
        string r = buffer;
		delete[] buffer;
		fclose(file);
        return r;
    }
    cerr<<"couldn't open "<<filename<<endl;
    return "";
}

#ifndef FLX_RPI

void ReshapeCallback(int width, int height)
{
    w=width;
    h=height;
}

void IdleCallback()
{
	glutPostRedisplay();
}


void glTranslatex(GLfixed x, GLfixed y, GLfixed z)
{
    glTranslatef(x/65536.0,y/65536.0,z/65536.0);
}

void glFrustumx(GLfixed xmin, GLfixed xmax, GLfixed ymin, GLfixed ymax, GLfixed zNear, GLfixed zFar)
{
    glFrustum(xmin/65536.0, xmax/65536.0,
              ymin/65536.0, ymax/65536.0,
              zNear/65536.0, zFar/65536.0);
}

void glClearColorx(GLfixed r, GLfixed g, GLfixed b, GLfixed a)
{
    glClearColor(r/65536.0,g/65536.0,b/65536.0,a/65536.0);
}

void glMaterialx( GLenum face, GLenum pname, GLfixed param)
{
    glMaterialf(face,pname,param/65536.0);
}

void glMaterialxv( GLenum face, GLenum pname, GLfixed * params)
{
    float fparams[4];
    fparams[0]=params[0]/65536.0;
    fparams[1]=params[1]/65536.0;
    fparams[2]=params[2]/65536.0;
    fparams[3]=params[3]/65536.0;
    glMaterialfv(face,pname,fparams);
}

void glLightxv( GLenum light, GLenum pname, GLfixed * params)
{
    float fparams[4];
    fparams[0]=params[0]/65536.0;
    fparams[1]=params[1]/65536.0;
    fparams[2]=params[2]/65536.0;
    fparams[3]=params[3]/65536.0;
    glLightfv(light,pname,fparams);
}

void glMultMatrixx( GLfixed * mat )
{
    float m[16];
    for (int i=0; i<16; i++)
    {
        m[i]=mat[i]/65536.0f;
    }
    glMultMatrixf(m);
}

#endif // FLX_RPI

//// common //////////////////////////

void DisplayCallback()
{
    if (!pthread_mutex_trylock(render_mutex)) {

#ifdef FLX_RPI
  appRender(state->screen_width, state->screen_height);
  eglSwapBuffers(state->display, state->surface);
#else
  appRender(w, h);
  glutSwapBuffers();
#endif

  pthread_mutex_unlock(render_mutex);
    } //else { printf("locked\n"); }
}

#ifdef FLX_RPI

static void init_ogl_rpi(RPI_STATE_T *state)
{
   int32_t success = 0;
   EGLBoolean result;
   EGLint num_config;

   static EGL_DISPMANX_WINDOW_T nativewindow;

   DISPMANX_ELEMENT_HANDLE_T dispman_element;
   DISPMANX_DISPLAY_HANDLE_T dispman_display;
   DISPMANX_UPDATE_HANDLE_T dispman_update;
   VC_RECT_T dst_rect;
   VC_RECT_T src_rect;

   static const EGLint attribute_list[] =
   {
      EGL_RED_SIZE, 8,
      EGL_GREEN_SIZE, 8,
      EGL_BLUE_SIZE, 8,
      EGL_ALPHA_SIZE, 8,
      EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
      EGL_NONE
   };

   EGLConfig config;

   // get an EGL display connection
   state->display = eglGetDisplay(EGL_DEFAULT_DISPLAY);
   assert(state->display!=EGL_NO_DISPLAY);

   // initialize the EGL display connection
   result = eglInitialize(state->display, NULL, NULL);
   assert(EGL_FALSE != result);

   // get an appropriate EGL frame buffer configuration
   result = eglChooseConfig(state->display, attribute_list, &config, 1, &num_config);
   assert(EGL_FALSE != result);

   // create an EGL rendering context
   state->context = eglCreateContext(state->display, config, EGL_NO_CONTEXT, NULL);
   assert(state->context!=EGL_NO_CONTEXT);

   // create an EGL window surface
   success = graphics_get_display_size(0 /* LCD */, &state->screen_width, &state->screen_height);
   assert( success >= 0 );

   dst_rect.x = 0;
   dst_rect.y = 0;
   dst_rect.width = state->screen_width;
   dst_rect.height = state->screen_height;

   src_rect.x = 0;
   src_rect.y = 0;
   src_rect.width = state->screen_width << 16;
   src_rect.height = state->screen_height << 16;

   dispman_display = vc_dispmanx_display_open( 0 /* LCD */);
   dispman_update = vc_dispmanx_update_start( 0 );

   dispman_element = vc_dispmanx_element_add ( dispman_update, dispman_display,
      0/*layer*/, &dst_rect, 0/*src*/,
					       &src_rect, DISPMANX_PROTECTION_NONE, 0 /*alpha*/, 0/*clamp*/, 0/*transform*/);

   nativewindow.element = dispman_element;
   nativewindow.width = state->screen_width;
   nativewindow.height = state->screen_height;
   vc_dispmanx_update_submit_sync( dispman_update );

   state->surface = eglCreateWindowSurface( state->display, config, &nativewindow, NULL );
   assert(state->surface != EGL_NO_SURFACE);

   // connect the context to the surface
   result = eglMakeCurrent(state->display, state->surface, state->surface, state->context);
   assert(EGL_FALSE != result);

   // Set background color and clear buffers
   glClearColor(0.f, 0.f, 0.f, 0.5f);
   glClear( GL_COLOR_BUFFER_BIT );
   glClear( GL_DEPTH_BUFFER_BIT );
   //glShadeModel(GL_FLAT);

   // Enable back face culling.
   // glEnable(GL_CULL_FACE);
}

#endif

void repl_loop() {
    char cmd_str[80];
    do {
        printf("fluxus> ");
        fgets( cmd_str, 80, stdin );
        pthread_mutex_lock(render_mutex);
        appEval(cmd_str);
        pthread_mutex_unlock(render_mutex);
    } while(1);
}

/////////////// osc stuff ////////////////////////////////////////////////

void osc_error_handler(int num, const char *msg, const char *path)
{
    printf("liblo server error %d in path %s\n",num,path);
}

int osc_default_handler(const char *path, const char *types, lo_arg **argv, int argc, void *data, void *user_data)
{
    //printf("osc server no handler for: %s %s\n",path,types);
	return 1;
}

int osc_eval_handler(const char *path, const char *types, lo_arg **argv,
		    int argc, void *data, void *user_data)
{
    if (types[0]=='s') {
        printf("%s\n",argv[0]);
        pthread_mutex_lock(render_mutex);
        appEval((char*)argv[0]);
        pthread_mutex_unlock(render_mutex);
    }
	return 1;
}

void setup_osc_repl() {
    printf("starting osc, listening to port 8000\n");
	lo_server_thread server = lo_server_thread_new("8000", osc_error_handler);
    lo_server_thread_add_method(server, NULL, NULL, osc_default_handler, NULL);
    lo_server_thread_add_method(server, "/eval", "s", osc_eval_handler, NULL);
	lo_server_thread_start(server);
}

/////////////////////////////////////////////////////////////////////////

int main(int argc, char *argv[])
{
#ifdef FLX_RPI
   bcm_host_init();

   // Clear application state
   memset( state, 0, sizeof( *state ) );

   init_ogl_rpi(state);
#else
   w=640;
   h=480;

   unsigned int flags = GLUT_DOUBLE|GLUT_RGBA|GLUT_DEPTH;

   // init OpenGL
   glutInit(&argc,argv);
   glutInitWindowSize(w,h);
   glutInitDisplayMode(flags);
   char windowtitle[256];
   sprintf(windowtitle,"jellyfish");
   glutCreateWindow(windowtitle);
   glutDisplayFunc(DisplayCallback);
   glutIdleFunc(IdleCallback);
#endif

    appInit();
    initGL();

    //appEval((char*)LoadFile("material/flx/init.scm").c_str());
    //appEval((char*)LoadFile("material/flx/boot.scm").c_str());

    appEval((char*)LoadFile(ASSETS_LOCATION+"init.scm").c_str());
    appEval((char*)LoadFile(ASSETS_LOCATION+"boot.scm").c_str());
    appEval((char*)LoadFile(ASSETS_LOCATION+"lib.scm").c_str());
    appEval((char*)LoadFile(ASSETS_LOCATION+"compiler.scm").c_str());

    // preload the textures
    long w=0,h=0;
    unsigned char *tex=LoadPNG(ASSETS_LOCATION+"raspberrypi.png",w,h);
    appLoadTexture("raspberrypi.png",w,h,(char *)tex);
    tex=LoadPNG(ASSETS_LOCATION+"stripes.png",w,h);
    appLoadTexture("stripes.png",w,h,(char *)tex);
    tex=LoadPNG(ASSETS_LOCATION+"thread.png",w,h);
    appLoadTexture("thread.png",w,h,(char *)tex);

    //    appEval((char*)LoadFile(ASSETS_LOCATION+"jellyfish.scm").c_str())

    if (argc>1) {
      appEval((char*)LoadFile(string(argv[1])).c_str());
    }

    // setup the repl thread
	render_mutex = new pthread_mutex_t;
	pthread_mutex_init(render_mutex,NULL);
    pthread_t *repl_thread = new pthread_t;
    pthread_create(repl_thread,NULL,(void*(*)(void*))repl_loop,NULL);

    setup_osc_repl();

#ifdef FLX_RPI
  while (!terminate_prog)
   {
      //usleep(5*1000);
     DisplayCallback();
   }
#else
	glutMainLoop();
#endif

	return 0;
}
