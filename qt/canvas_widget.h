 #ifndef CANVASWIDGET_H
 #define CANVASWIDGET_H

#include <QWidget>
#include <string>
#include "drag_widget.h"
#include "brick_widget.h"

 class QDragEnterEvent;
 class QDropEvent;

 class canvas_widget : public drag_widget
 {
 public:
     canvas_widget(QWidget *parent = 0);
     virtual void add(const std::string &text, QPoint pos, QPoint offset);

     static brick_widget *build_from_code(drag_widget *parent, const std::string &code);
     std::string convert_to_code();

 protected:
     void mousePressEvent(QMouseEvent *event);

 };

 #endif
