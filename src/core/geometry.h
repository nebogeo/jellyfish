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

#include "vec3.h"
#include "list.h"

#ifndef FLX_GEOMETRY
#define FLX_GEOMETRY

// returns the parametric distance along the line, or -1 for no intersection
flx_real intersect_line_triangle(const vec3 &start, const vec3 &end,
                                 const vec3 &ta, const vec3 &tb, const vec3 &tc,
                                 vec3 &bary);

class intersect_point : public list::node
{
public:
    float m_t;
    list m_blends;

    class blend : public list::node
    {
    public:
        blend() : m_name(NULL) {};
        ~blend() { free(m_name); }
        char *m_name;
        vec3 m_blend;
    };
};

// need to delete the point
intersect_point *interpolate_pdata(const list *pdata, float t, const vec3 &bary, u32 i1, u32 i2, u32 i3);

#endif
