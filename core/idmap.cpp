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

#include <stdlib.h>
#include <string.h>
#include "idmap.h"

idmap::idmap() {}
idmap::~idmap() {}

void idmap::add(char *name, int id)
{
    idmap_node *nm = new idmap_node;
    nm->m_name=strdup(name);
    nm->m_id=id;
    m_map.add_to_front(nm);
}

int idmap::get(char *name)
{
    list::node *p=m_map.m_head;
    while (p!=NULL)
    {
        idmap_node *mn = (idmap_node*)p;
        
        if (!strcmp(name,mn->m_name))
            return mn->m_id;
        
        p=p->m_next;
    }
    return 0;
}

