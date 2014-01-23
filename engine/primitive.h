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

#include "importgl.h"
#include "../core/fixed.h"
#include "../core/list.h"
#include "../core/vec3.h"
#include "../core/mat44.h"
#include "../core/pdata.h"

#ifndef FLX_PRIMITIVE
#define FLX_PRIMITIVE

class primitive
{
public:
    enum type {TRIANGLES,TRISTRIP,POINTS,LINES,LINESTRIP};
    primitive(unsigned int size, type t);
    primitive();
    ~primitive();

    virtual void build();

    virtual void render(u32 hints);

    void make_cube();
    void set_colour(flx_real r, flx_real g,
                    flx_real b, flx_real a);
    void apply(const mat44 &m);
    unsigned int pdata_size() { return m_size; }
    virtual void pdata_add(const char* name, vec3 *ptr);
    vec3 *get_pdata_arr(const char* name);
    void pdata_set(const char* name, int i, vec3 v);
    vec3 *pdata_get(const char* name, int i);

    // delete my list please, see geometry.h:points
    list *intersect(const vec3 &start, const vec3 &end);
    bool intersect_fast(const vec3 &start, const vec3 &end);

protected:

    // delete my list please
    list *intersect_tristrip(const vec3 &start, const vec3 &end);
    bool intersect_tristrip_fast(const vec3 &start, const vec3 &end);

    vec3 *m_positions;
    vec3 *m_colours;
    vec3 *m_normals;
    vec3 *m_tex;
    u8* m_colours_;
    u32 m_type;
    u32 m_size;
    list m_pdata;
};

#endif
