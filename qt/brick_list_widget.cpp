#include <QtGui>
#include <QString>
#include <string>
#include <iostream>
#include "brick_list_widget.h"
#include "sexpr.h"
#include "canvas_widget.h"

using namespace std;

brick_list_widget::brick_list_widget(const string &text, QWidget *parent)
  : brick_widget(parent)
{
  // set depth based on parent depth
  brick_list_widget *dwparent = dynamic_cast<brick_list_widget *>(parent);
  if (dwparent) m_depth=dwparent->get_depth()+1;
  m_main = new QVBoxLayout();
  setLayout(m_main);

  //QPushButton *flipb = new QPushButton("-");
  //m_main->addWidget(flipb);
  //QObject::connect(flipb, SIGNAL(released()), this, SLOT(flip_me()));

  //setup for list interface
  m_label = new QLineEdit(QString::fromStdString(text));
  m_label->setStyleSheet("border: 0px;");
  m_label->setFrame(false);
  m_main->addWidget(m_label);
  m_container = new QHBoxLayout();
  m_main->addLayout(m_container);
  //     setStyleSheet("background-color:grey;");
  //     setGeometry(0,0,300,100);

  setSizePolicy(QSizePolicy::Fixed,QSizePolicy::Fixed);

  setMinimumSize(0, 0);
  m_main->setSpacing(0);
  m_main->setMargin(0);
  m_main->setContentsMargins(20,2,2,2);
  unsigned int r=(m_depth+1)*50%255;
  unsigned int g=(m_depth+1)*10%255;
  unsigned int b=(m_depth+1)*204%255;

  if (m_selected) {
    r=255; g=255; b=0;
  };

  setStyleSheet(QString("border-radius: 8px; border: 2px solid black; color:white; background:rgb(%0,%1,%2);")
                .arg(r).arg(g).arg(b));

  //if (rand()%2==0)
  flip();

}

void brick_list_widget::flip() {
  QBoxLayout *t = new QVBoxLayout();

  for (int i=0; i<m_container->count(); ++i) {
    QWidget *widget = m_container->itemAt(i)->widget();
    m_container->removeWidget(widget);
    t->addWidget(widget);
  }
  delete m_container;
  m_container = t;
  m_main->addLayout(m_container);
  resize(sizeHint().width(),sizeHint().height());
}

string brick_list_widget::code() {
  // we are a list...
  string name = m_label->displayText().toUtf8().constData();
  string ret="(";
  if (name!="") ret+=name+" ";

  for (int i=0; i<m_container->count(); ++i) {
    brick_list_widget *widget = static_cast<brick_list_widget*>(m_container->itemAt(i)->widget());
    if (widget != NULL) {
      if (i>0) ret+=" ";
      ret+=widget->code();
    }
  }
  ret+=")";
  return ret;
}

void brick_list_widget::add(const string &text, QPoint pos, QPoint offset) {
  add(canvas_widget::build_from_code(this,text));
}

void brick_list_widget::add(brick_widget *dw) {
  m_container->addWidget(dw);
  //  m_root->resize(m_root->sizeHint().width(),m_root->sizeHint().height());
  resize(sizeHint().width(),sizeHint().height());

  //m_root->delete_up();
  //  recur_resize();
  dw->show();
  dw->setAttribute(Qt::WA_DeleteOnClose);
}

void brick_list_widget::resize_down() {
  resize(sizeHint().width(),sizeHint().height());
  for (int i=0; i<m_container->count(); ++i) {
    brick_list_widget *widget = static_cast<brick_list_widget*>(m_container->itemAt(i)->widget());
    widget->resize_down();
  }
}

void brick_list_widget::delete_up() {
  for (int i=0; i<m_container->count(); ++i) {
    brick_list_widget *widget = static_cast<brick_list_widget*>(m_container->itemAt(i)->widget());
    widget->delete_up();
  }
  if (m_delete_me) {
    close();
    deleteLater();
  }
}
