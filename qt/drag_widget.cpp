#include <QtGui>
#include <iostream>

#include "drag_widget.h"

using namespace std;

drag_widget::drag_widget(QWidget *parent)
  : QWidget(parent) {
  setAcceptDrops(true);
}

void drag_widget::dragEnterEvent(QDragEnterEvent *event) {
  if (event->mimeData()->hasFormat("application/x-schemebricks")) {
    if (children().contains(event->source())) {
      event->setDropAction(Qt::MoveAction);
      event->accept();
    } else {
      event->acceptProposedAction();
    }
  } else if (event->mimeData()->hasText()) {
    event->acceptProposedAction();
  } else {
    event->ignore();
  }
}

void drag_widget::dragMoveEvent(QDragMoveEvent *event) {
  if (event->mimeData()->hasFormat("application/x-schemebricks")) {
    if (children().contains(event->source())) {
      event->setDropAction(Qt::MoveAction);
      event->accept();
    } else {
      event->acceptProposedAction();
    }
  } else if (event->mimeData()->hasText()) {
    event->acceptProposedAction();
  } else {
    event->ignore();
  }
}

void drag_widget::dropEvent(QDropEvent *event) {
  if (event->mimeData()->hasFormat("application/x-schemebricks")) {
    const QMimeData *mime = event->mimeData();
    QByteArray itemData = mime->data("application/x-schemebricks");
    QDataStream dataStream(&itemData, QIODevice::ReadOnly);
    QString text;
    QPoint offset;
    dataStream >> text >> offset;
    add(text.toUtf8().constData(),event->pos(), offset);

    if (event->source() == this) {
      event->setDropAction(Qt::MoveAction);
      event->accept();
    } else {
      event->acceptProposedAction();
    }
  } else {
    //event->ignore();

    if (event->mimeData()->hasText()) {
      const QMimeData *mime = event->mimeData();
      QByteArray itemData = mime->data("application/x-schemebricks");
      QDataStream dataStream(&itemData, QIODevice::ReadOnly);
      QString text;
      QPoint offset;
      dataStream >> text;
      cerr<<text.toUtf8().constData()<<endl;
      //add(text.toUtf8().constData(),event->pos(), offset);
    } else {
      event->ignore();
    }

  }

}

QPoint drag_widget::global_pos() {
  drag_widget *p = dynamic_cast<drag_widget*>(parentWidget());
  if (p!=NULL) {
    return pos()+p->global_pos();
  } else {
    return QPoint(0,0);
  }
}

// needed so we can apply the style
void drag_widget::paintEvent(QPaintEvent *) {
  QStyleOption opt;
  opt.init(this);
  QPainter p(this);
  style()->drawPrimitive(QStyle::PE_Widget, &opt, &p, this);
}
