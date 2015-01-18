// Copyright (C) 2011 Dave Griffiths
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

#include <stdlib.h>
#include <math.h>
#include <float.h>
#include <assert.h>

#include "importgl.h"
#include "app.h"
#include "engine/engine.h"
#include "scheme/scheme.h"

scheme *sc=NULL;;
FILE *log_file=NULL;

// Called from the app framework.
void appInit()
{
    sc=scheme_init_new();

    #ifdef FLX_LINUX
    FILE *log_file=stdout;
    scheme_set_input_port_file(sc, stdin);
    #else
    #ifdef FLX_RPI
    FILE *log_file=stdout;
    scheme_set_input_port_file(sc, stdin);
    #else
    FILE *log_file=fopen("/sdcard/symbai/symbai-log2.txt","w");
    #endif
    #endif
    if (log_file!=NULL) scheme_set_output_port_file(sc, log_file);

    fprintf(log_file,"testing...\n");
    fflush(log_file);
}

void initGL()
{
    int a;

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
}

// Called from the app framework.
void appDeinit()
{
    fclose(log_file);
    int a;
}


static void gluPerspective(GLfloat fovy, GLfloat aspect,
                           GLfloat zNear, GLfloat zFar)
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


static void prepareFrame(int width, int height)
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
    gluPerspective(45, (float)width / height, 0.5f, 150);

    glMatrixMode(GL_MODELVIEW);

    glLoadIdentity();
}


static void configureLightAndMaterial()
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

// Called from the app framework.
/* The tick is current time in milliseconds, width and height
 * are the image dimensions to be rendered.
 */
void appRender(int width, int height)
{
    // Prepare OpenGL ES for rendering of the frame.
    prepareFrame(width, height);

    // Configure environment.
    configureLightAndMaterial();

    scheme_load_string(sc,"(frame-hook)");

    glPushMatrix();
    engine::get()->render();
    glPopMatrix();
}

void appEval(char *code)
{
    scheme_load_string(sc,code);
    //fflush(log_file);
}

u32 appLoadTexture(const char *filename,int width, int height, char* data)
{
    return engine::get()->load_texture(filename,width,height,(u8*)data);
}
