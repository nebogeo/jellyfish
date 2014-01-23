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
#include "importgl.h"
#include "texture.h"

u32 texture_manager::load(const char *name, u32 w, u32 h, u8* data)
{
    texture_node *new_node = new texture_node;

    glGenTextures(1,&new_node->id);
    new_node->name=strdup(name);
    new_node->width=w;
    new_node->height=h;

    glBindTexture(GL_TEXTURE_2D,new_node->id);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, data);

    m_textures.add_to_end(new_node);

    return new_node->id;
}

u32 texture_manager::find(const char *name)
{
    texture_node *n=static_cast<texture_node*>(m_textures.m_head);
    while (n!=NULL)
    {
        if (!strcmp(n->name,name))
        {
            return n->id;
        }
        n=static_cast<texture_node*>(n->m_next);
    }
    return 0;
}

void texture_manager::apply(u32 id)
{
    glBindTexture(GL_TEXTURE_2D, id);
}
