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

#include "../core/list.h"
#include "../core/mat44.h"
#include "primitive.h"

#ifndef FLX_SCENENODE
#define FLX_SCENENODE

#define HINT_NONE           0x00000000
#define HINT_SOLID          0x00000001
#define HINT_WIRE           0x00000002
#define HINT_NORMAL         0x00000004
#define HINT_POINTS         0x00000008
#define HINT_AALIAS         0x00000010
#define HINT_BOUND          0x00000020
#define HINT_UNLIT          0x00000040
#define HINT_VERTCOLS       0x00000080
#define HINT_ORIGIN         0x00000100
#define HINT_CAST_SHADOW    0x00000200
#define HINT_IGNORE_DEPTH   0x00000400
#define HINT_DEPTH_SORT     0x00000800
#define HINT_LAZY_PARENT    0x00001000
#define HINT_CULL_CCW       0x00002000
#define HINT_WIRE_STIPPLED  0x00004002 // this also turns on HINT_WIRE
#define HINT_SPHERE_MAP     0x00008000
#define HINT_FRUSTUM_CULL   0x00010000
#define HINT_NORMALISE      0x00020000
#define HINT_NOBLEND        0x00040000
#define HINT_NOZWRITE       0x00080000

class scenenode : public list::node 
{
public:
    scenenode(primitive *p);
    ~scenenode();

    scenenode *find_child(int id);
    void remove_child(int id);
    
    u32 m_id;
    primitive *m_primitive;
    scenenode *m_parent;
    list m_children;

    mat44 m_tx;
    u32 m_hints;
    u32 m_line_width;
    u32 m_texture;
};

#endif
