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

class Graph;

#ifndef FLX_ENGINE
#define FLX_ENGINE

class obj_reader;

class engine
{
public:
    engine();
    ~engine();

    // singleton crap
    static void init();
    static engine *get();

    void push();
    void pop();
    void grab(int id);
    void ungrab();

    void identity();
    void translate(float x, float y, float z);
    void scale(float x, float y, float z);
    void rotate(float x, float y, float z);
    void colour(float r, float g, float b, float a);
    void hint(u32 hint);
    void line_width(u32 w);
    void texture(u32 id);
    void clear();
    void destroy(int id);
    void text_set(const char *str);
    mat44 *get_transform();
    mat44 get_global_transform();
    mat44 *get_camera_transform() { return &m_camera_tx; }
    void set_screensize(unsigned int w, unsigned int h) { m_screen_width=w; m_screen_height=h; }
    unsigned int *get_screensize() { return &m_screen_width; }
    void apply_transform();

    unsigned int pdata_size();
    void pdata_add(const char *name);
    vec3 *pdata_get(const char *name, int i);
    void pdata_set(const char *name, int i, vec3 v);

    int build_cube();
    int load_obj(char *fn);
    int raw_obj(char *data);
    int build_obj(obj_reader &reader);
    int build_text(char *str);
    int build_polygons(unsigned int size, int type);
    int build_jellyfish(u32 size);
    void parent(int p);
    void clear_colour(float r, float g, float b, float a)
    {
        m_clear_r=r;
        m_clear_g=g;
        m_clear_b=b;
        m_clear_a=a;
    }

    void render_immediate(int id);

    u32 load_texture(const char *filename, u32 w, u32 h, u8* data);
    u32 get_texture(const char *filename);
    list *geo_line_intersect(const vec3 &start, const vec3 &end);
    u32 get_line_intersect(const vec3 &start, const vec3 &end);

    void render();

    float m_clear_r,m_clear_g,m_clear_b,m_clear_a;

    Graph *get_audio_graph() { return m_audio_graph; }

private:

    list m_state_stack;
    list m_grab_stack;

    class state_stack_item : public list::node
    {
    public:
        state_stack_item();
        state_stack_item(const state_stack_item &other);

        mat44 m_tx;
        u32 m_parent;
        float m_colr,m_colg,m_colb,m_cola;
        u32 m_hints;
        u32 m_line_width;
        u32 m_texture;
    };

    class grab_stack_item : public list::node
    {
    public:
        int m_id;
    };

    state_stack_item *state_top();
    bool grabbed();
    int grabbed_id();
    scenenode *grabbed_node();
    void setup_state(scenenode *n);

    static engine *m_engine;
    scenegraph *m_sg;
    Graph *m_audio_graph;
    unsigned int m_screen_width;
    unsigned int m_screen_height;
    mat44 m_camera_tx;
    //mat44 m_inv_camera_tx;
};

#endif
