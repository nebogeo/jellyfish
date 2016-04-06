#include <QtGui>
#include <QString>
#include <string>
#include <iostream>
#include "brick_atom_widget.h"
#include "sexpr.h"

using namespace std;

brick_atom_widget::brick_atom_widget(const string &code, QWidget *parent)
  : brick_widget(parent)
{
  // set depth based on parent depth
  brick_widget *dwparent = dynamic_cast<brick_widget *>(parent);
  if (dwparent) m_depth=dwparent->get_depth()+1;

  build(code,parent);
}

void brick_atom_widget::build(const string &code, QWidget *parent) {
  m_main = new QVBoxLayout();
  setLayout(m_main);

  // build atom
  m_label = new QLineEdit(QString::fromStdString(code));
  m_label->setFont(QFont("Courier Bold", 9));
  m_label->setFrame(false);
  m_main->addWidget(m_label);
  // dont want to drop into an atom
  setAcceptDrops(false);

  m_label->setSizePolicy(QSizePolicy::Fixed,QSizePolicy::Fixed);
  setSizePolicy(QSizePolicy::Fixed,QSizePolicy::Fixed);
  m_label->setMinimumSize(QSize(10,10));
  setMinimumSize(QSize(10,10));

  setMinimumSize(0, 0);
  m_main->setSpacing(0);
  m_main->setMargin(0);
  m_main->setContentsMargins(10,5,5,5);
  unsigned int r=(m_depth+1)*5%255;
  unsigned int g=(m_depth+1)*20%255;
  unsigned int b=(m_depth+1)*300%255;

  if (m_selected) {
    r=255; g=255; b=0;
  };

  setStyleSheet(QString("border: 2px solid black; color:white; background:rgb(%1,%2,%3);")
                .arg(r).arg(g).arg(b));
  m_label->setStyleSheet(QString("border: 1px solid black; color:white; background:rgb(%1,%2,%3);")
                .arg(r).arg(g).arg(b));
}
