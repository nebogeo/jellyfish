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
#include "types.h"
#include "list.h"

#ifndef BB_IDMAP
#define BB_IDMAP

class idmap
{
public:
    idmap();
    ~idmap();

    void add(char *name, int id);
    int get(char *name);

    class idmap_node: public list::node
    {
    public:
        virtual ~idmap_node() 
        {
            free(m_name);
        }
        char *m_name;
        int m_id;
    };
        
private:
    list m_map;
};

#endif
