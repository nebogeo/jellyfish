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

#include <string>

#include "engine/importgl.h"
#include "engine/engine.h"
#include "core/fixed.h"
#include "core/pixels.h"

#include "graphics.h"
#include "interpreter.h"

using namespace std;

const string graphics::INPUT_CALLBACK="fluxus-input-callback";
const string graphics::INPUT_RELEASE_CALLBACK="fluxus-input-release-callback";
int graphics::m_frame_num = 0;
pthread_mutex_t* graphics::m_render_mutex = NULL;
int graphics::m_w = 0;
int graphics::m_h = 0;

void graphics::initialise()
{
    glEnable(GL_DEPTH_TEST);
    glDisable(GL_CULL_FACE);
    glShadeModel(GL_SMOOTH);
    glEnable(GL_BLEND);

    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);
    glDisable(GL_LIGHT1);
    glDisable(GL_LIGHT2);

    glEnableClientState(GL_VERTEX_ARRAY);

    engine::init();

    // setup the repl thread
    m_render_mutex = new pthread_mutex_t;
    pthread_mutex_init(m_render_mutex,NULL);

}

// in case we don't have glu/glut
void graphics::glu_perspective(GLfloat fovy, GLfloat aspect, GLfloat zNear, GLfloat zFar)
{
    GLfloat xmin, xmax, ymin, ymax;

    ymax = zNear * (GLfloat)tan(fovy * 3.141 / 360);
    ymin = -ymax;
    xmin = ymin * aspect;
    xmax = ymax * aspect;

    glFrustumx((GLfixed)(xmin * 65536), (GLfixed)(xmax * 65536),
               (GLfixed)(ymin * 65536), (GLfixed)(ymax * 65536),
               (GLfixed)(zNear * 65536), (GLfixed)(zFar * 65536));
}


void graphics::prepare_frame(int width, int height)
{
    glViewport(0, 0, width, height);

    engine::get()->set_screensize(width,height);

//    glClearColorx((GLfixed)(engine::get()->m_clear_r * 65536),
//                  (GLfixed)(engine::get()->m_clear_g * 65536),
//                  (GLfixed)(engine::get()->m_clear_b * 65536),
//                  (GLfixed)(0.5 * 65536));
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glu_perspective(45, (float)width / height, 0.5f, 150);

    glMatrixMode(GL_MODELVIEW);

    glLoadIdentity();
}


void graphics::configure_light_and_material()
{
    static GLfixed light0Position[] = { -0x40000, 0x100000, 0x00000, 0 };
    static GLfixed light0Diffuse[] = { 0x10000, 0x10000, 0x10000, 0x10000 };
    static GLfixed light1Position[] = { 0x10000, -0x20000, -0x10000, 0 };
    static GLfixed light1Diffuse[] = { 0x11eb, 0x23d7, 0x5999, 0x10000 };
    static GLfixed light2Position[] = { -0x10000, 0, -0x40000, 0 };
    static GLfixed light2Diffuse[] = { 0x11eb, 0x2b85, 0x23d7, 0x10000 };
    static GLfixed materialSpecular[] = { 0x10000, 0x10000, 0x10000, 0x10000 };

    glLightxv(GL_LIGHT0, GL_POSITION, light0Position);
    glLightxv(GL_LIGHT0, GL_DIFFUSE, light0Diffuse);
    glLightxv(GL_LIGHT1, GL_POSITION, light1Position);
    glLightxv(GL_LIGHT1, GL_DIFFUSE, light1Diffuse);
    glLightxv(GL_LIGHT2, GL_POSITION, light2Position);
    glLightxv(GL_LIGHT2, GL_DIFFUSE, light2Diffuse);
//    glMaterialxv(GL_FRONT_AND_BACK, GL_SPECULAR, materialSpecular);

//    glMaterialx(GL_FRONT_AND_BACK, GL_SHININESS, 60 << 16);
    glEnable(GL_COLOR_MATERIAL);
}

void graphics::render(int width, int height)
{
    // Prepare OpenGL ES for rendering of the frame.
    prepare_frame(width, height);

    // Configure environment.
    configure_light_and_material();

    interpreter::eval("(frame-hook)");

    glPushMatrix();
    engine::get()->render();
    glPopMatrix();
}

u32 graphics::load_texture_from_file(const string &path, const string &filename) {
    long w,h=0;
    unsigned char *tex=load_png(path+filename,w,h);
    return load_texture_raw(filename,w,h,(char *)tex);
}


u32 graphics::load_texture_raw(const string &texname,int width, int height, char* data) {
    return engine::get()->load_texture(texname.c_str(),width,height,(u8*)data);
}


// GLUT stuff follows

void graphics::display_callback() {
    if (!pthread_mutex_trylock(m_render_mutex)) {

#ifdef FLX_RPI
        appRender(state->screen_width, state->screen_height);
        eglSwapBuffers(state->display, state->surface);
#else
        graphics::render(m_w, m_h);
        glutSwapBuffers();
#endif

#ifdef USE_JPGLIB
        static char fn[256];
        sprintf(fn,"shot-%0.4d.jpg",m_frame_num);
        cerr<<fn<<endl;
        WriteJPG(GetScreenBuffer(0, 0, m_w, m_h, 1),fn,"",0,0,m_w,m_h,95,1);
        m_frame_num++;
#endif

        pthread_mutex_unlock(m_render_mutex);
    } //else { printf("locked\n"); }
}

void graphics::keyboard_callback(unsigned char key, int x, int y)
{
	char code[256];
#ifdef FLX_RPI
	int imod = 0;
	sprintf(code,"(%s #\\%c %d %d %d %d %d %d)",INPUT_CALLBACK.c_str(),key,-1,-1,-1,x,y,imod);
    interpreter::eval(code);
#else
	int mod = glutGetModifiers();
	if (key > 0 && key<0x80) {
        // key is 0 on ctrl+2 and ignore extended ascii for the time being
		int imod = 0;
		if (mod & GLUT_ACTIVE_SHIFT) mod |= 1;
		if (mod & GLUT_ACTIVE_CTRL)	imod |= 2;
		if (mod & GLUT_ACTIVE_ALT) imod |= 4;
		sprintf(code,"(%s #\\%c %d %d %d %d %d %d)",INPUT_CALLBACK.c_str(),key,-1,-1,-1,x,y,imod);
        interpreter::eval(code);
	}
#endif
}

void graphics::keyboard_up_callback(unsigned char key,int x, int y) {
  char code[256];
  if (key > 0 && key<0x80) {
    // key is 0 on ctrl+2
    sprintf(code,"(%s #\\%c %d %d %d %d %d %d)",INPUT_RELEASE_CALLBACK.c_str(),key,-1,-1,-1,x,y,0);
    interpreter::eval(code);
  }
}

#ifdef FLX_LINUX

void graphics::reshape_callback(int width, int height) {
    m_w=width;
    m_h=height;
}

void graphics::idle_callback() {
	glutPostRedisplay();
}

void graphics::mouse_callback(int button, int state, int x, int y) {
    char code[256];
	snprintf(code,256,"(%s %d %d %d %d %d %d %d)",INPUT_CALLBACK.c_str(),0,button,-1,state,x,y,0);
	interpreter::eval(code);
}

void graphics::motion_callback(int x, int y) {
    char code[256];
	snprintf(code,256,"(%s %d %d %d %d %d %d %d)",INPUT_CALLBACK.c_str(),0,-1,-1,-1,x,y,0);
	interpreter::eval(code);
}

void graphics::passive_motion_callback(int x, int y) {
    char code[256];
	snprintf(code,256,"(%s %d %d %d %d %d %d %d)",INPUT_CALLBACK.c_str(),0,-1,-1,-1,x,y,0);
	interpreter::eval(code);
}


#endif
