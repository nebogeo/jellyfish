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
#include <pthread.h>

#include "engine/importgl.h"
#include "core/fixed.h"
#include "core/pixels.h"
#include "core/osc.h"
#include "app.h"
#include "audio/alsa.h"

#ifdef FLX_RPI
#include <assert.h>
#include "bcm_host.h"
#include "rpi/input.h"
#include "rpi/graphics.h"
#endif

#ifndef FLX_RPI
#include "linux/glut_graphics.h"
#endif

using namespace std;

int w,h=0;
int gAppAlive = 1;
int modifiers = 0;
pthread_mutex_t* render_mutex;
static const string INPUT_CALLBACK="fluxus-input-callback";
static const string INPUT_RELEASE_CALLBACK="fluxus-input-release-callback";

// setup the assets location
// on linux, mimic android, otherwise load locally on rpi
#ifndef ASSETS_LOCATION
#ifdef ANDROID_NDK
static const string ASSETS_LOCATION("../assets/");
#else
static const string ASSETS_LOCATION("assets/");
#endif
#endif

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

//// common //////////////////////////

static int frame_num=0;

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

#ifdef USE_JPGLIB

  static char fn[256];
  sprintf(fn,"shot-%0.4d.jpg",frame_num);
  cerr<<fn<<endl;
  WriteJPG(GetScreenBuffer(0, 0, w, h, 1),
           fn,"",0,0,w,h,95,1);
  frame_num++;
#endif

  pthread_mutex_unlock(render_mutex);
    } //else { printf("locked\n"); }
}

// pasted from nomadic engine...??
void KeyboardCallback(unsigned char key,int x, int y)
{
	char code[256];
#ifdef FLX_RPI
	int imod = 0;
	sprintf(code,"(%s #\\%c %d %d %d %d %d %d)",INPUT_CALLBACK.c_str(),key,-1,-1,-1,x,y,imod);
	appEval(code);
#else
	int mod=modifiers;
	mod=glutGetModifiers();
	if (key > 0 && key<0x80)
	{ // key is 0 on ctrl+2 and ignore extended ascii for the time being
		int imod = 0;
		if (mod & GLUT_ACTIVE_SHIFT)
			imod |= 1;
		if (mod & GLUT_ACTIVE_CTRL)
			imod |= 2;
		if (mod & GLUT_ACTIVE_ALT)
			imod |= 4;

		sprintf(code,"(%s #\\%c %d %d %d %d %d %d)",INPUT_CALLBACK.c_str(),key,-1,-1,-1,x,y,imod);
        appEval(code);
	}
#endif
}

void ReshapeCallback(int width, int height)
{
    w=width;
    h=height;
}

void IdleCallback()
{
	glutPostRedisplay();
}

void KeyboardUpCallback(unsigned char key,int x, int y)
{
  char code[256];
  if (key > 0 && key<0x80) {
    // key is 0 on ctrl+2
    sprintf(code,"(%s #\\%c %d %d %d %d %d %d)",INPUT_RELEASE_CALLBACK.c_str(),key,-1,-1,-1,x,y,0);
    appEval(code);
  }
}

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
   glutKeyboardFunc(KeyboardCallback);
   glutKeyboardUpFunc(KeyboardUpCallback);
#endif

   appInit();
   initGL();

   appEval((char*)LoadFile(ASSETS_LOCATION+"init.scm").c_str());
   appEval((char*)LoadFile(ASSETS_LOCATION+"boot.scm").c_str());
   appEval((char*)LoadFile(ASSETS_LOCATION+"lib.scm").c_str());
   appEval((char*)LoadFile(ASSETS_LOCATION+"compiler.scm").c_str());
   appEval((char*)LoadFile(ASSETS_LOCATION+"fluxa.scm").c_str());

    // preload the textures
    long w=0,h=0;
    unsigned char *tex=LoadPNG(ASSETS_LOCATION+"raspberrypi.png",w,h);
    appLoadTexture("raspberrypi.png",w,h,(char *)tex);
    tex=LoadPNG(ASSETS_LOCATION+"stripes.png",w,h);
    appLoadTexture("stripes.png",w,h,(char *)tex);
    tex=LoadPNG(ASSETS_LOCATION+"bg.png",w,h);
    appLoadTexture("bg.png",w,h,(char *)tex);
    tex=LoadPNG(ASSETS_LOCATION+"thread.png",w,h);
    appLoadTexture("thread.png",w,h,(char *)tex);

    if (argc>1) {
        appEval((char*)LoadFile(string(argv[1])).c_str());
    }

    // setup the repl thread
    render_mutex = new pthread_mutex_t;
    pthread_mutex_init(render_mutex,NULL);
    pthread_t *repl_thread = new pthread_t;
    //pthread_create(repl_thread,NULL,(void*(*)(void*))repl_loop,NULL);
    //setup_osc_repl();

#ifdef FLX_RPI
    //	getMouse();
    //	getKeys();

    /*  while (!terminate_prog)
   {
      doEvents(state->screen_width, state->screen_height,
	       KeyboardCallback,
	       KeyboardUpCallback);

      //usleep(5*1000);
     DisplayCallback();
     }*/
#else
	glutMainLoop();
#endif

	return 0;
}
