// Copyright (C) 2011 Dave Griffiths
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

#include "instance_primitive.h"
#include "../core/fixed.h"
#include "../core/geometry.h"
#include "../engine/scenenode.h"
#include "../core/msg.h"

instance_primitive::instance_primitive(primitive *src):
  m_src(src) {
}

void instance_primitive::build() {
}

instance_primitive::~instance_primitive() {
}


void instance_primitive::render(u32 hints) {
    m_src->render(hints);
}
