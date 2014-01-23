// Copyright (C) 2010 Dave Griffiths
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

#include "list.h"

using namespace bb;

list::list() : m_head(NULL)
{
}

list::~list()
{
    clear();
}

void list::clear()
{
    node *p=m_head;
    while (p!=NULL)
    {
        node *t=p->m_next;
        delete p;
        p=t;
    }
    m_head=NULL;
}

u32 list::size()
{
    u32 s=0;
    node *p=m_head;
    while (p!=NULL)
    {
        s++;
        p=p->m_next;
    }
    return s;
}

list::node *list::last()
{
    node *p=m_head;
    while (p!=NULL && p->m_next!=NULL) p=p->m_next;
    return p;
}

void list::add_to_front(node* n)
{
    n->m_next=m_head;
    m_head=n;
}

void list::add_to_end(node* n)
{
    n->m_next=NULL;
    if (m_head==NULL) m_head=n;
    else last()->m_next=n;
}

// you need to delete returned node
list::node *list::remove_from_front()
{
    if (m_head==NULL) return NULL;
    node *ret=m_head;
    m_head=m_head->m_next;
    return ret;
}

// you need to delete n
void list::remove(node* n)
{
    node *parent=m_head;
    bool found=false;
    if (parent==n) remove_from_front();

    while(parent!=NULL && !found)
    {
        if (parent->m_next==n) found=true;
        else parent=parent->m_next;
    }

    if (parent!=NULL)
    {
        parent->m_next=n->m_next;
    }
}

s32 list::unit_test()
{
    list l;
    if (l.m_head!=NULL) return 1;
    if (l.size()!=0) return 2;
    node *n1=new node();
    l.add_to_front(n1);
    if (l.size()!=1) return 3;
    if (l.m_head==NULL) return 4;
    if (l.last()!=n1) return 5;
    node *n2=new node();
    l.add_to_end(n2);
    if (l.last()!=n2) return 6;
    if (l.size()!=2) return 7;
    l.clear();
    if (l.size()!=0) return 8;
    if (l.last()!=NULL) return 9;
    return 0;
}
