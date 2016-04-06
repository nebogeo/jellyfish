#ifndef BRICKWIDGET_H
#define BRICKWIDGET_H

#include <QWidget>
#include <QLabel>
#include <QLineEdit>
#include <QVBoxLayout>
#include <string>
#include "drag_widget.h"

 class brick_widget : public drag_widget
 {
    Q_OBJECT
 public:
     brick_widget(QWidget *parent = 0);

     virtual std::string code()=0;

     void set_depth(unsigned int d) { m_depth=d; }
     unsigned int get_depth() { return m_depth; }

 protected:
     //void mousePressEvent(QMouseEvent *event);
     void mouseDoubleClickEvent(QMouseEvent * e);

     unsigned int m_depth;
     bool m_delete_me;
     bool m_selected;
 };

 #endif
