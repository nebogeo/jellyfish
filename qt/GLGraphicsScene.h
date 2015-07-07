#ifndef GLGRAPHICSSCENE_H
#define GLGRAPHICSSCENE_H

#include <QGraphicsScene>
#include <QtOpenGL>
#include <QPainter>
#include <QRectF>

class GLGraphicsScene : public QGraphicsScene {
    Q_OBJECT

public:
    GLGraphicsScene();
    void drawBackground(QPainter* painter, const QRectF& rect);

private slots:
    void frame_tick();
};

#endif // GLGRAPHICSSCENE_H
