#include "engine/importgl.h"

// fixed point versions for speeeed
void glTranslatex(GLfixed x, GLfixed y, GLfixed z);
void glFrustumx(GLfixed xmin, GLfixed xmax, GLfixed ymin, GLfixed ymax, GLfixed zNear, GLfixed zFar);
void glClearColorx(GLfixed r, GLfixed g, GLfixed b, GLfixed a);
void glMaterialx( GLenum face, GLenum pname, GLfixed param);
void glMaterialxv( GLenum face, GLenum pname, GLfixed * params);
void glLightxv( GLenum light, GLenum pname, GLfixed * params);
void glMultMatrixx( GLfixed * mat );
