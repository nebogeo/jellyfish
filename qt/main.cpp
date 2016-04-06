#include <QtGui>
#include <QtOpenGL/QGLWidget>

#include "GLGraphicsScene.h"
#include "SyntaxHighlight.h"
#include "MainWindow.h"

int main( int argc , char *argv[] ){
    QApplication app(argc, argv);

    MainWindow mainWin;
    mainWin.show();

    return app.exec();
}
