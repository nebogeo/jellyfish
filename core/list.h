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

#ifndef BB_LIST
#define BB_LIST

namespace bb {

class list
{
public:
    list();
    ~list();

    class node
    {
    public:
        node() : m_next(NULL) {}
//        virtual ~node() {} <- crashes when deleting primitives, probably not deleting it now?
        node *m_next;
    };

    void clear();
    u32 size();
    bool empty() { return size()>0; }
    node *last();
    void add_to_front(node* n);
    void add_to_end(node* n);
    // you need to delete returned node
    node *remove_from_front();
    // you need to delete n
    void remove(node* n);

    static s32 unit_test();

    node* m_head;
};

}

#endif
