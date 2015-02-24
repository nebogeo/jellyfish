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

#ifdef FLX_RPI
#include <assert.h>
#include "bcm_host.h"
#include "rpi/input.h"
#include "rpi/graphics.h"
#else
#include "linux/glut_graphics.h"
#endif

using namespace std;

int w,h=0;
int gAppAlive = 1;
int modifiers = 0;
pthread_mutex_t* render_mutex;
static const string INPUT_CALLBACK="fluxus-input-callback";
static const string INPUT_RELEASE_CALLBACK="fluxus-input-release-callback";

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

#ifdef FLX_LINUX

void ReshapeCallback(int width, int height)
{
    w=width;
    h=height;
}

void IdleCallback()
{
	glutPostRedisplay();
}

void MouseCallback(int button, int state, int x, int y)
{
    char code[256];
	snprintf(code,256,"(%s %d %d %d %d %d %d %d)",INPUT_CALLBACK.c_str(),0,button,-1,state,x,y,0);
	appEval(code);
}

void MotionCallback(int x, int y)
{
    char code[256];
	snprintf(code,256,"(%s %d %d %d %d %d %d %d)",INPUT_CALLBACK.c_str(),0,-1,-1,-1,x,y,0);
	appEval(code);
}

void PassiveMotionCallback(int x, int y)
{
    char code[256];
	snprintf(code,256,"(%s %d %d %d %d %d %d %d)",INPUT_CALLBACK.c_str(),0,-1,-1,-1,x,y,0);
	appEval(code);
}


#endif

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
        printf("jellyfish> ");
        fgets( cmd_str, 80, stdin );
        pthread_mutex_lock(render_mutex);
        appEval(cmd_str);
        pthread_mutex_unlock(render_mutex);
    } while(1);
}

int main(int argc, char *argv[])
{
    bool window=true;
    if (argc>=2 && !strcmp(argv[1],"-nw")) window=false;

#ifdef FLX_RPI
   bcm_host_init();
   // Clear application state
   memset( state, 0, sizeof( *state ) );
    if (window) {
   init_ogl_rpi(state);
#else
    if (window) {
   w=800;
   h=600;

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
   glutMouseFunc(MouseCallback);
   glutMotionFunc(MotionCallback);
   glutPassiveMotionFunc(PassiveMotionCallback);
#endif
    }
   appInit();

   if (window) {
       initGL();
   }


   appEval((char*)LoadFile(string(ASSETS_PATH)+"init.scm").c_str());
   appEval((char*)LoadFile(string(ASSETS_PATH)+"boot.scm").c_str());
   if (window) appEval((char*)LoadFile(string(ASSETS_PATH)+"fluxus.scm").c_str());
   appEval((char*)LoadFile(string(ASSETS_PATH)+"lib.scm").c_str());
   appEval((char*)LoadFile(string(ASSETS_PATH)+"compiler.scm").c_str());
   appEval((char*)LoadFile(string(ASSETS_PATH)+"fluxa.scm").c_str());

   if (window) {
    // preload the textures
    long w=0,h=0;
    unsigned char *tex=LoadPNG(string(ASSETS_PATH)+"raspberrypi.png",w,h);
    appLoadTexture("raspberrypi.png",w,h,(char *)tex);
    tex=LoadPNG(string(ASSETS_PATH)+"stripes.png",w,h);
    appLoadTexture("stripes.png",w,h,(char *)tex);
    tex=LoadPNG(string(ASSETS_PATH)+"bg.png",w,h);
    appLoadTexture("bg.png",w,h,(char *)tex);
    tex=LoadPNG(string(ASSETS_PATH)+"thread.png",w,h);
    appLoadTexture("thread.png",w,h,(char *)tex);
    tex=LoadPNG(string(ASSETS_PATH)+"oolite-font.png",w,h);
    appLoadTexture("oolite-font.png",w,h,(char *)tex);
   }

    if (argc>1) {
        appEval((char*)LoadFile(string(argv[argc-1])).c_str());
    }

    // setup the repl thread
    render_mutex = new pthread_mutex_t;
    pthread_mutex_init(render_mutex,NULL);
    pthread_t *repl_thread = new pthread_t;
    pthread_create(repl_thread,NULL,(void*(*)(void*))repl_loop,NULL);
    setup_osc_repl();

#ifdef FLX_RPI
    getMouse();
    getKeys();

   while (!terminate_prog)
   {
      doEvents(state->screen_width, state->screen_height,
	       KeyboardCallback,
	       KeyboardUpCallback);

      //usleep(5*1000);
      if (window) DisplayCallback();
      else {
          if (!pthread_mutex_trylock(render_mutex)) {
              appEval("(frame-hook)");
              pthread_mutex_unlock(render_mutex);
          }
      }
      usleep(1000);
     }
#else
	if (window) glutMainLoop();
	else
    {
        while(true)
        {
            if (!pthread_mutex_trylock(render_mutex)) {
                appEval("(frame-hook)");
                usleep(1000);
                pthread_mutex_unlock(render_mutex);
            }
        }
    }
#endif

	return 0;
}
