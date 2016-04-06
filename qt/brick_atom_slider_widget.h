#ifndef BRICKATOMSLIDERWIDGET_H
#define BRICKATOMSLIDERWIDGET_H

#include <QWidget>
#include <QLineEdit>
#include <QVBoxLayout>
#include <string>
#include "brick_atom_widget.h"

class brick_atom_slider_widget : public brick_atom_widget {
 public:
  brick_atom_slider_widget(const std::string &code, QWidget *parent = 0);

  virtual void add(const std::string &text, QPoint pos, QPoint offset) {}

  virtual std::string code() { return m_label->displayText().toUtf8().constData(); }

  virtual void build(const std::string &code, QWidget *parent=0);

 protected:

     QLineEdit *m_label;
     QVBoxLayout *m_main;
 };

 #endif
