#include <QtGui>
#include <QString>
#include <string>
#include <iostream>
#include "brick_widget.h"
#include "sexpr.h"
#include "interpreter.h"

using namespace std;

brick_widget::brick_widget(QWidget *parent)
  : drag_widget(parent),
    m_depth(0),
    m_delete_me(false),
    m_selected(false)
{
}

void brick_widget::mouseDoubleClickEvent(QMouseEvent * e) {
  if (e->button() == Qt::RightButton) {
    string c = code();
    cerr<<"eval: "<<c<<endl;
    interpreter::eval(c);
  }
}
