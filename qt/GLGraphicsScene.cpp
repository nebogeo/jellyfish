#include <QTimer>
#include "GLGraphicsScene.h"

#include <iostream>

#include "graphics.h"
#include "interpreter.h"

using namespace std;

GLGraphicsScene::GLGraphicsScene() {

    QTimer *timer = new QTimer(this);
    connect(timer, SIGNAL(timeout()), this, SLOT(frame_tick()));
    timer->start(1000);
    timer->setInterval(10);

}

void GLGraphicsScene::frame_tick() {
    update();
}

static bool first = true;

void GLGraphicsScene::drawBackground(QPainter* painter, const QRectF& rect) {
    //Q_UNUSED(rect);

    if (first) {
        interpreter::initialise();
        graphics::initialise();

        interpreter::eval_file(string(ASSETS_PATH)+"init.scm");
        interpreter::eval_file(string(ASSETS_PATH)+"boot.scm");
        interpreter::eval_file(string(ASSETS_PATH)+"fluxus.scm");
        interpreter::eval_file(string(ASSETS_PATH)+"lib.scm");
        interpreter::eval_file(string(ASSETS_PATH)+"compiler.scm");
        interpreter::eval_file(string(ASSETS_PATH)+"fluxa.scm");

        graphics::load_texture_from_file(string(ASSETS_PATH),"raspberrypi.png");
        graphics::load_texture_from_file(string(ASSETS_PATH),"stripes.png");
        graphics::load_texture_from_file(string(ASSETS_PATH),"bg.png");
        graphics::load_texture_from_file(string(ASSETS_PATH),"thread.png");
        graphics::load_texture_from_file(string(ASSETS_PATH),"oolite-font.png");

        first = false;
    }

    bool m_MotionBlur=false;

	if (m_MotionBlur)
	{
		glEnable(GL_COLOR_MATERIAL);
		glPolygonMode(GL_FRONT,GL_FILL);
		glDisable(GL_DEPTH_TEST);
		glPushMatrix();
		glTranslatef(0,0,-10);
		glBegin(GL_QUADS);
        glColor4f(0,0,0,0.1f);
			glVertex3f(-10,-10,0);
			glVertex3f(10,-10,0);
			glVertex3f(10,10,0);
			glVertex3f(-10,10,0);
		glEnd();
		glPopMatrix();
		glEnable(GL_DEPTH_TEST);
		glDisable(GL_COLOR_MATERIAL);
	} else {
        glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
        glClear(GL_COLOR_BUFFER_BIT);
    }

    // on the safe side...
    glPushAttrib(GL_ALL_ATTRIB_BITS);
    graphics::render(rect.width(),rect.height());
    glPopAttrib();

}
