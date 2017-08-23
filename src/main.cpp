// Copyright (C) 2015 Dave Griffiths
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
#include <unistd.h>

#include "core/osc.h"
#include "interpreter.h"
#include "graphics.h"

using namespace std;

#ifdef FLX_RPI
#include <assert.h>
#include "bcm_host.h"
#include "rpi/input.h"
#include "rpi/graphics.h"
static volatile int terminate_prog;
static RPI_STATE_T _state, *state=&_state;
#endif

int main(int argc, char *argv[])
{
  bool window=true;
  bool repl=false;
  if (argc>=2 && !strcmp(argv[1],"-nw")) window=false;
  if (argc>=2 && !strcmp(argv[1],"-r")) graphics::m_record=true;

#ifdef FLX_RPI
  bcm_host_init();
  // Clear application state
  memset( state, 0, sizeof( *state ) );
  if (window) {
    init_ogl_rpi(state);
    graphics::rpi_state = state;
    graphics::m_w=state->screen_width;
    graphics::m_h=state->screen_height;
  }
#else
  graphics::m_w=1024;
  graphics::m_h=768;

  if (window) {
    glutInit(&argc,argv);
    glutInitWindowSize(graphics::m_w,graphics::m_h);
    glutInitDisplayMode(GLUT_DOUBLE|GLUT_RGBA|GLUT_DEPTH);
    glutCreateWindow(PACKAGE_STRING);
    glutDisplayFunc(graphics::display_callback);
    glutIdleFunc(graphics::idle_callback);
    glutKeyboardFunc(graphics::keyboard_callback);
    glutKeyboardUpFunc(graphics::keyboard_up_callback);
    glutMouseFunc(graphics::mouse_callback);
    glutMotionFunc(graphics::motion_callback);
    glutPassiveMotionFunc(graphics::passive_motion_callback);
  }
#endif

  interpreter::initialise();
  if (window) graphics::initialise();
  else graphics::init_mutex();
  
  interpreter::eval_file(string(ASSETS_PATH)+"init.scm");
  interpreter::eval_file(string(ASSETS_PATH)+"boot.scm");
  if (window) interpreter::eval_file(string(ASSETS_PATH)+"fluxus.scm");
  interpreter::eval_file(string(ASSETS_PATH)+"lib.scm");
  interpreter::eval_file(string(ASSETS_PATH)+"compiler.scm");
  interpreter::eval_file(string(ASSETS_PATH)+"fluxa.scm");

  if (argc>1) interpreter::eval_file(argv[argc-1]);
  
  if (repl) interpreter::start_repl(graphics::m_render_mutex);
  network_osc::start_osc_repl(graphics::m_render_mutex);

#ifdef FLX_RPI
  //    getMouse();
  //getKeys();

  while (!terminate_prog) {
    //doEvents(state->screen_width, state->screen_height,
    //          graphics::keyboard_callback,
    //          graphics::keyboard_up_callback);
    
    //usleep(5*1000);
    if (window) graphics::display_callback();
    else {
      if (!pthread_mutex_trylock(graphics::m_render_mutex)) {
	interpreter::eval("(frame-hook)");
	pthread_mutex_unlock(graphics::m_render_mutex);
      }
    }
    usleep(1000);
  }
#else
  if (window) {
    glutMainLoop();
  } else {
    while(true) {
      if (!pthread_mutex_trylock(graphics::m_render_mutex)) {
	interpreter::eval("(frame-hook)");
	pthread_mutex_unlock(graphics::m_render_mutex);
	usleep(10);
      }
    }
  }
#endif

  return 0;
}
