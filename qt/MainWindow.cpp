#include <QtGui>

#include "MainWindow.h"
#include "GLGraphicsScene.h"
#include "interpreter.h"

#include <fstream>

using namespace std;

MainWindow::MainWindow()
{
    m_view = new QGraphicsView;
    m_view->setViewport(new QGLWidget(QGLFormat(QGL::SampleBuffers)));
    m_view->resize(800,600);
    m_view->setViewportUpdateMode(QGraphicsView::NoViewportUpdate);
    m_view->setScene(new GLGraphicsScene());
    setCentralWidget(m_view);
    setUnifiedTitleAndToolBarOnMac(true);

    QAction *run_act = new QAction(QIcon(":/images/new.png"), tr("&Run"), this);
    run_act->setShortcuts(QKeySequence::New);
    run_act->setStatusTip(tr("run"));
    connect(run_act, SIGNAL(triggered()), this, SLOT(run()));

    QAction *new_list_act = new QAction(QIcon(":/images/new.png"), tr("&new list"), this);
    new_list_act->setShortcuts(QKeySequence::New);
    new_list_act->setStatusTip(tr("new list"));
    connect(new_list_act, SIGNAL(triggered()), this, SLOT(new_list()));

    QAction *new_atom_act = new QAction(QIcon(":/images/new.png"), tr("&new atom"), this);
    new_atom_act->setShortcuts(QKeySequence::New);
    new_atom_act->setStatusTip(tr("new atom"));
    connect(new_atom_act, SIGNAL(triggered()), this, SLOT(new_atom()));

    QAction *save_act = new QAction(QIcon(":/images/save.png"), tr("save"), this);
    save_act->setShortcuts(QKeySequence::New);
    save_act->setStatusTip(tr("save"));
    connect(save_act, SIGNAL(triggered()), this, SLOT(save()));

    QAction *load_act = new QAction(QIcon(":/images/open.png"), tr("load"), this);
    load_act->setShortcuts(QKeySequence::New);
    load_act->setStatusTip(tr("load"));
    connect(load_act, SIGNAL(triggered()), this, SLOT(load()));


    QToolBar *fileToolBar = addToolBar(tr("File"));
    fileToolBar->addAction(run_act);
    fileToolBar->addAction(new_list_act);
    fileToolBar->addAction(new_atom_act);
    fileToolBar->addAction(save_act);
    fileToolBar->addAction(load_act);

    m_canvas = new canvas_widget();
    m_canvas->setStyleSheet("background: rgba(255,255,255,0%); color: white;");

    m_canvas->resize(800,600);
    m_canvas->add("(define (render) \
  (rotate (vector 45 45 45)) \
  (draw-cube))",QPoint(100,100),QPoint(0,0));
    m_canvas->add("(every-frame (render))",
                  QPoint(500,100),QPoint(0,0));

    m_view->scene()->addWidget(m_canvas);

}

void MainWindow::run() {


}

void MainWindow::new_list() {
    m_canvas->add("(list)",QPoint(10,10),QPoint(0,0));
}

void MainWindow::new_atom() {
    m_canvas->add("atom",QPoint(10,10),QPoint(0,0));
}

void MainWindow::load() {
  m_last_file=QFileDialog::getOpenFileName(this,
                                           QString("Load code"),
                                           m_last_file,
                                           QString("code (*.scmb)"));
}

void MainWindow::save() {
  m_last_file=QFileDialog::getSaveFileName(this,
                                           QString("Save scheme code"),
                                           m_last_file,
                                           QString("code (*.scm)"));

  ofstream os(m_last_file.toUtf8().constData());
  os<<m_canvas->convert_to_code();
  os.close();

}
