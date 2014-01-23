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

#include "scenenode.h"

scenenode::scenenode(primitive *p) :
    m_id(-1),
    m_primitive(p),
    m_parent(NULL),
    m_hints(HINT_SOLID),
    m_line_width(1),
    m_texture(0)
{
    if (m_primitive) m_primitive->build();
}

scenenode::~scenenode()
{
	if (m_parent!=NULL)
	{
		m_parent->remove_child(m_id);
	} 

    if (m_primitive!=NULL) delete m_primitive;
}

scenenode *scenenode::find_child(int id)
{
    scenenode *n=static_cast<scenenode*>(m_children.m_head);
    while (n!=NULL)
    {
        if (n->m_id==id) return n;
        n=static_cast<scenenode*>(n->m_next);
    }
    return NULL; 
}


void scenenode::remove_child(int id)
{
    scenenode *n=find_child(id);
    if (n!=NULL)
    {
        m_children.remove(n);
    }
}

