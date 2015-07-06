#include <QtGui>
#include <QtOpenGL/QGLWidget>

#include "GLGraphicsScene.h"
#include "SyntaxHighlight.h"
#include "MainWindow.h"

int main( int argc , char *argv[] ){

    QApplication app(argc, argv);

    MainWindow mainWin;
    mainWin.show();

/*    QGraphicsView view;
    view.setViewport(new QGLWidget(QGLFormat(QGL::SampleBuffers)));
    view.resize(800,600);
    view.setViewportUpdateMode(QGraphicsView::FullViewportUpdate);
    view.setScene(new GLGraphicsScene());

    QFont font;
    font.setFamily("Courier New");
    font.setFixedPitch(true);
    font.setPointSize(20);

    QHBoxLayout *main_layout = new QHBoxLayout;

    QTextEdit editor;
    editor.setWindowOpacity(0.8);
    editor.setStyleSheet("background-color: black; color: white;");
    editor.setText("(define (render n)\n    (draw-cube))\n(every-frame (render))");
    editor.setFont(font);

    SyntaxHighlight *highlighter = new SyntaxHighlight(editor.document());

    QTextEdit repl;
    repl.setWindowOpacity(0.8);
    repl.setStyleSheet("background-color: black; color: white;");
    repl.setText("repl>");
    repl.setFont(font);

    main_layout->setSpacing(0);
    main_layout->setMargin(0);
    main_layout->setContentsMargins(0,0,0,0);
    main_layout->addWidget(&editor);
    main_layout->addWidget(&repl);

    QDialog *d = new QDialog();
    d->setLayout(main_layout);
    d->setWindowOpacity(0.8);

    QString name = QString("&File");
    QMenu *fileMenu = new QMenu(name, app.activeWindow());
    app.activeWindow()->menuBar()->addMenu(fileMenu);

    fileMenu->addAction("&New", d, NULL, QKeySequence::New);


    view.scene()->addWidget(d,Qt::Window);
    view.show();
*/
    return app.exec();
}
