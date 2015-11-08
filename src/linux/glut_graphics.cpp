#include <iostream>
#include "engine/importgl.h"
#include "linux/glut_graphics.h"

using namespace std;

#ifndef FLX_RPI

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

#endif
