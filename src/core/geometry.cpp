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

#include "types.h"
#include "geometry.h"
#include "pdata.h"

// returns the parametric distance along the line, or -1 for no intersection
flx_real intersect_line_triangle(const vec3 &start, const vec3 &end, 
                              const vec3 &ta, const vec3 &tb, 
                              const vec3 &tc, vec3 &bary)
{
    vec3 u = ta-tc;
    vec3 v = tb-tc;
    vec3 n = v.cross(u);

    if (n.mag()==0.0f) return -1.0f;
    
    vec3 ray = end-start;
    vec3 w0 = start-tc;
    flx_real a = -n.dot(w0);
    flx_real b = n.dot(ray);

    // if b is small, ray is parallel
    if (b==0.0f) return -1.0f;
    //     if a==0 then the ray is in the plane

    flx_real r = a/b;
    if (r<0.0f) return -1.0f;
    if (r>1.0f) return -1.0f;
    vec3 I = start+(ray*r);
    flx_real uu = u.dot(u);
    flx_real uv = u.dot(v);
    flx_real vv = v.dot(v);
    vec3 w = I-tc;
    flx_real wu = w.dot(u);
    flx_real wv = w.dot(v);
    flx_real D = uv*uv - uu*vv;

    bary.x=(uv*wv - vv*wu)/D;
    if (bary.x<0.0f || bary.x>1.0f) return -1;
    bary.y=(uv*wu - uu*wv)/D;
    if (bary.y<0.0f || bary.y>1.0f) return -1;
    bary.z=1.0f-(bary.x+bary.y);
	if (bary.z<0.0f || bary.z>1.0f) return -1;

    return r;
}

// need to delete the point
intersect_point *interpolate_pdata(const list *pdata, float t, const vec3 &bary, u32 i1, u32 i2, u32 i3)
{
    intersect_point *p=new intersect_point;
    p->m_t = t;

    pdata_arr *n=static_cast<pdata_arr*>(pdata->m_head);
    while (n!=NULL)
    {
        intersect_point::blend *b = new intersect_point::blend;
        b->m_blend=n->m_array[i1]*bary.x +
            n->m_array[i2]*bary.y +
            n->m_array[i3]*bary.z;
        b->m_name=strdup(n->m_name);
        p->m_blends.add_to_front(b);
        n=static_cast<pdata_arr*>(n->m_next);
    }

	return p;
} 
