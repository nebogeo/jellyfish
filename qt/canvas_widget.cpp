#include <QtGui>
#include <string>

#include "canvas_widget.h"
#include "brick_widget.h"
#include "brick_atom_widget.h"
#include "brick_list_widget.h"
#include "sexpr.h"
#include <iostream>

using namespace std;

canvas_widget::canvas_widget(QWidget *parent)
  : drag_widget(parent) {

}

void canvas_widget::add(const string &text, QPoint pos, QPoint offset) {
  cerr<<"building"<<endl;
  brick_widget *w = build_from_code(this,text);
  cerr<<"built "<<text<<endl;
  w->move(pos - offset);
  //addWidget(w);
  w->show();
  cerr<<"shown "<<w<<endl;
  //w->setAttribute(Qt::WA_DeleteOnClose);
}

// assume well formatted atm
brick_widget *canvas_widget::build_from_code(drag_widget *parent, const string &code) {
  if (code[0]=='(') {

    // skip (
    string inner = code.substr(1,sexpr::count_paren(code)-1);
    vector<string> tokens = sexpr::tokenise(inner);
    string name = "";
    if (tokens.size()>0) {
      name = tokens[0];
      cerr<<"setting name to: "<<tokens[0]<<endl;
      tokens.erase(tokens.begin());
    }
    brick_list_widget *w = new brick_list_widget(name,parent);

    //cerr<<"inner-: "<<inner<<endl;
    for (vector<string>::iterator i=tokens.begin(); i!=tokens.end(); ++i) {
      //cerr<<"token-: "<<*i<<endl;
      w->add(build_from_code(w,*i));
    }

    //if (tokens.size()>2) w->flip();

    return w;

  } else {
    //    cerr<<"atom-: "<<code<<endl;
    brick_atom_widget *w = new brick_atom_widget(code,parent);
    return w;
  }

}

string canvas_widget::convert_to_code() {
  string ret;
  foreach(QObject *qw, children()) {
    brick_widget *w = dynamic_cast<brick_widget*>(qw);
    if (w) {
      QPoint p = w->pos();
      char buf[256];
      snprintf(buf,256,";; pos: %d %d",p.x(),p.y());
      ret+=buf;
      ret+="\n";
      ret+=w->code();
      ret+="\n";
    }
  }
  return ret;
}

void canvas_widget::mousePressEvent(QMouseEvent *event) {
  if (event->button() == Qt::LeftButton) {

  brick_widget *child = dynamic_cast<brick_widget*>(childAt(event->pos()));
  if (!child) {
    cerr<<"canvas: no child"<<endl;
    return;
  }

  QPoint hotSpot = event->pos() - child->global_pos();
  QByteArray itemData;
  QDataStream dataStream(&itemData, QIODevice::WriteOnly);
  //cerr<<"sent ["<<child->code()<<"]"<<endl;
  dataStream << QString::fromStdString(child->code()) << hotSpot;
  QMimeData *mimeData = new QMimeData;
  mimeData->setData("application/x-schemebricks", itemData);

  QDrag *drag = new QDrag(this);
  drag->setMimeData(mimeData);
  drag->setPixmap(QPixmap::grabWidget(child));
  drag->setHotSpot(hotSpot);
  child->hide();

  if (drag->exec(Qt::MoveAction | Qt::CopyAction, Qt::CopyAction)) {
    child->close();
    child->deleteLater();
  } else {
    // drag failed
    child->show();
  }
  }
}
