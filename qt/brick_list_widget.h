// Copyright (C) 2015 Foam Kernow
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#ifndef BRICKLISTWIDGET_H
#define BRICKLISTWIDGET_H

#include <QWidget>
#include <QLabel>
#include <QLineEdit>
#include <QVBoxLayout>
#include <string>
#include "brick_widget.h"

class brick_list_widget : public brick_widget {
    Q_OBJECT
 public:
    brick_list_widget(const std::string &text, QWidget *parent = 0);

     virtual void add(const std::string &text, QPoint pos, QPoint offset);

     void add(brick_widget *dw);
     virtual std::string code();
     void flip();

 private slots:

     void flip_me() {
        flip();
     }

 protected:

     void resize_down();
     void delete_up();

     QLineEdit *m_label;
     QBoxLayout *m_main;
     QBoxLayout *m_container;
 };

 #endif
