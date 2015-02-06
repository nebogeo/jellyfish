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
#include "texture.h"

#ifndef FLX_SCENEGRAPH
#define FLX_SCENEGRAPH

class scenegraph
{
public:
    scenegraph();
    ~scenegraph();

    /// adds a node onto a parent node (0 is the root)
    int add(int pid, scenenode *n);
    /// adds immediate mode node referencing existing primitive, 
    /// will be removed after rendering
    void add_immediate(scenenode *n);
    /// delete a node and it's children
    void remove(int id);
	/// finds a node in the tree from its ID
    scenenode *find(int id) const;
	/// moves a node (and all it's children) around the graph
    void reparent(int id, int pid);
	/// clear the tree
    void clear();
	/// is the child attached to the parent?
	bool is_attached_to(scenenode *parent, scenenode *n) const;
	scenenode *root() { return m_root; }    
    mat44 get_global_transform(scenenode *n);

    void render();
    void dump() const;

    texture_manager m_texture_manager;
    
    // returns the first node intersecting with the line
    u32 intersect_node_walk(scenenode *node, const vec3 &start, const vec3 &end);


protected:
	scenenode* find_node_walk(scenenode *n, int id) const;
	void dump_walk(scenenode *n, int d) const;
    void render_node_walk(scenenode *n, int depth);
	scenenode *m_root;
	scenenode *m_immediate_root;
    int m_current_id;
};

#endif
