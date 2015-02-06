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

#include "scenegraph.h"

#ifdef _EE
#include "ee/ps2-renderer.h"
#endif

scenegraph::scenegraph() :
    m_root(NULL),
    m_immediate_root(NULL),
    m_current_id(0)
{
	clear();
}

scenegraph::~scenegraph()
{
    delete m_root;
}

void scenegraph::clear()
{
    if (m_root!=NULL)
    {
        delete m_root;
        m_root=NULL;
    }
	m_root=new scenenode(NULL);
    m_root->m_id=0;
    m_current_id=1;

    if (m_immediate_root!=NULL)
    {
        delete m_immediate_root;
        m_immediate_root=NULL;
    }
	m_immediate_root=new scenenode(NULL);
    m_immediate_root->m_id=9999;
}

int scenegraph::add(int pid, scenenode *node)
{
    scenenode *parent=find(pid);
    if (!parent) parent=m_root;
    node->m_id=m_current_id++;
    parent->m_children.add_to_front(node);
    node->m_parent=parent;
    return node->m_id;
}

void scenegraph::add_immediate(scenenode *node)
{
    scenenode *parent=m_immediate_root;
    node->m_id=9999;
    parent->m_children.add_to_front(node);
    node->m_parent=parent;
}

void scenegraph::remove(int id)
{
    scenenode *n=find(id);
    if (n!=NULL) {
        if (n->m_parent!=NULL){
            n->m_parent->remove_child(id);
        }
        delete n;
    }
}

scenenode *scenegraph::find(int id) const
{
    return find_node_walk(m_root,id);
}

scenenode *scenegraph::find_node_walk(scenenode *node, int id) const
{
	if (node==NULL) return NULL;
    if (node->m_id==id) return node;

    scenenode *n=static_cast<scenenode*>(node->m_children.m_head);
    while (n!=NULL)
    {
        scenenode *r=find_node_walk(n,id);
        if (r!=NULL) return r;
        n=static_cast<scenenode*>(n->m_next);
    }

	return NULL;
}

void scenegraph::dump() const
{
    dump_walk(m_root,0);
}

void scenegraph::dump_walk(scenenode *node, int d) const
{
	if (node==NULL) return;

//    for (int i=0; i<d; i++) cerr<<"   ";

//    cerr<<node->m_id<<endl;

    scenenode *n=static_cast<scenenode*>(node->m_children.m_head);
    while (n!=NULL)
    {
        dump_walk(n,d+1);
        n=static_cast<scenenode*>(n->m_next);
    }
}

/// Moves a node (and all it's children) around the graph
void scenegraph::reparent(int id, int pid)
{
	scenenode *node = find(id);
	scenenode *new_parent = find(pid);

	if (node==NULL) return;
	if (new_parent==NULL) return;

    scenenode *old_parent=node->m_parent;
	old_parent->remove_child(id);
    new_parent->m_children.add_to_front(node);
	node->m_parent=new_parent;
}

bool scenegraph::is_attached_to(scenenode *parent, scenenode *child) const
{
/*	Node *current = Child;

	// iterate back up the tree looking at parents...
	while(current!=NULL)
	{
		if (current==Parent)
		{
			return true;
		}
		current=current->Parent;
        }*/
	return false;
}

void scenegraph::render()
{
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ZERO /*GL_ONE_MINUS_SRC_ALPHA*/);

    if (m_root!=NULL)
    {
        render_node_walk(m_root,1);
    }

    // render immediate mode
    render_node_walk(m_immediate_root,1);

    // clear immediate mode
    delete m_immediate_root;
	m_immediate_root=new scenenode(NULL);
    m_immediate_root->m_id=9999;
}

void scenegraph::render_node_walk(scenenode *node, int depth)
{
#ifdef _EE
    ps2_renderer::get()->push_matrix();
    ps2_renderer::get()->mult_matrix(node->m_tx.arr());
#else
	glPushMatrix();
    glMultMatrixx((GLfixed*)&node->m_tx.m[0][0]);
//    glMultMatrixf(&node->m_tx.m[0][0]);
#endif

#ifndef _EE
//    glDisable(GL_LIGHTING);

    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);

    // global render hints settings
    if (node->m_hints&HINT_UNLIT) glDisable(GL_LIGHTING);
    else glEnable(GL_LIGHTING);

    // not supported on gles
    //glLineWidth(node->m_line_width);
    //glPointSize(node->m_line_width); // ok to share this??
	if (node->m_hints&HINT_NOZWRITE) glDepthMask(false);
    else glDepthMask(true);
	if (node->m_hints&HINT_IGNORE_DEPTH) glDisable(GL_DEPTH_TEST);
    else glEnable(GL_DEPTH_TEST);

    glEnable(GL_NORMALIZE);

#endif
    if (node->m_texture!=0)
    {
        glEnable(GL_TEXTURE_2D);
        m_texture_manager.apply(node->m_texture);
    }
    else glDisable(GL_TEXTURE_2D);

    if (node->m_primitive!=NULL)
    {
        node->m_primitive->render(node->m_hints);
    }

    depth++;
    scenenode *n=static_cast<scenenode*>(node->m_children.m_head);
    while (n!=NULL)
    {
        render_node_walk(n,depth);
        n=static_cast<scenenode*>(n->m_next);
    }

#ifdef _EE
    ps2_renderer::get()->pop_matrix();
#else
	glPopMatrix();
#endif
}

u32 scenegraph::intersect_node_walk(scenenode *node,  const vec3 &start, const vec3 &end)
{
    if (node->m_primitive!=NULL)
    {
        // transform the line into local space
        mat44 inv=get_global_transform(node).inverse();
        vec3 ls=inv.transform(start);
        vec3 le=inv.transform(end);

        // run the intersection
        if (node->m_primitive->intersect_fast(ls,le))
        {
            return node->m_id;
        }
    }

    scenenode *n=static_cast<scenenode*>(node->m_children.m_head);
    while (n!=NULL)
    {
        u32 r=intersect_node_walk(n,start,end);
        if (r!=0) return r;
        n=static_cast<scenenode*>(n->m_next);
    }
    return 0;
}


mat44 scenegraph::get_global_transform(scenenode *node)
{
	mat44 mat;
	scenenode* current=node;
	while(current!=NULL)
	{
		mat*=current->m_tx;
		current=static_cast<scenenode*>(current->m_parent);
	}
	return mat;
}
