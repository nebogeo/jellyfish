#include <QtGui>
#include "canvas_widget.h"

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow();

private slots:
  void run();
  void new_list();
  void new_atom();
  void load();
  void save();

protected:

private:

    canvas_widget *m_canvas;
    QGraphicsView *m_view;
    QString m_last_file;
};
