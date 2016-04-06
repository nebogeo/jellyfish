 #ifndef DRAGWIDGET_H
 #define DRAGWIDGET_H

#include <QWidget>
#include <string>

 class QDragEnterEvent;
 class QDropEvent;

 class drag_widget : public QWidget
 {
    Q_OBJECT
 public:
     drag_widget(QWidget *parent = 0);

     virtual void add(const std::string &text, QPoint pos, QPoint offset)=0;
     QPoint global_pos();

 protected:

     // needed so we can apply the style
     void paintEvent(QPaintEvent *);

     void dragEnterEvent(QDragEnterEvent *event);
     void dragMoveEvent(QDragMoveEvent *event);
     virtual void dropEvent(QDropEvent *event);
 };

 #endif
