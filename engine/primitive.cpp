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

#include "primitive.h"
#include "../core/fixed.h"
#include "../core/geometry.h"
#include "../engine/scenenode.h"

#ifdef _EE
#include "ee/ps2-renderer.h"
#endif

#include "cube.h"

primitive::primitive(unsigned int size, type t)
{
    #ifndef _EE
    switch (t)
    {
    case TRIANGLES: m_type=GL_TRIANGLES; break;
    case TRISTRIP: m_type=GL_TRIANGLE_STRIP; break;
    default: m_type=GL_TRIANGLES;
    }
    #endif

    // store data contiguously
    m_size=size;
}

// (called in scenenode ctr)
void primitive::build()
{
    // todo: EE code: (vec3*)memalign(128, sizeof(vec3) * m_size
    vec3 *data = new vec3[m_size*4];

    pdata_add("p",data);
    pdata_add("n",data+m_size);
    pdata_add("c",data+m_size*2);
    pdata_add("t",data+m_size*3);
    m_colours_=new u8[4*m_size];
    m_positions=get_pdata_arr("p");
    m_normals=get_pdata_arr("n");
    m_colours=get_pdata_arr("c");
    m_tex=get_pdata_arr("t");
}

primitive::primitive()
{
    m_size=12*3;
#ifndef _EE
    m_type=GL_TRIANGLES;
#endif

    vec3* data = new vec3[m_size*4];
    pdata_add("p",data);
    pdata_add("n",data+m_size);
    pdata_add("c",data+m_size*2);
    pdata_add("t",data+m_size*3);

    m_colours_=new u8[4*m_size];

    m_positions=get_pdata_arr("p");
    m_normals=get_pdata_arr("n");
    m_colours=get_pdata_arr("c");
    m_tex=get_pdata_arr("t");

    int p=0;
    for (int i=0; i<m_size; i++)
    {
        int index = cube_triangles[p];

        m_positions[i].x = cube_vertices[index*3];
        m_positions[i].y = cube_vertices[index*3+1];
        m_positions[i].z = cube_vertices[index*3+2];

        int nindex = cube_normal_indices[p];

        m_normals[i].x = cube_normals[nindex*3];
        m_normals[i].y = cube_normals[nindex*3+1];
        m_normals[i].z = cube_normals[nindex*3+2];

        m_colours[i].x = cube_colours[nindex*4];
        m_colours[i].y = cube_colours[nindex*4+1];
        m_colours[i].z = cube_colours[nindex*4+2];

        m_tex[i].x = cube_vertices[index*3]/2+0.5;
        m_tex[i].y = cube_vertices[index*3+1]/2+0.5;
        m_tex[i].z = 0;

        //m_colours[i*4+3] = 255;

        p++;
    }
}

primitive::~primitive()
{
    delete m_colours_;
}

void primitive::pdata_add(const char* name, vec3 *ptr)
{
    if (get_pdata_arr(name)!=NULL) return;
#ifdef _EE
    m_pdata.add_to_front(new pdata_arr(name, (vec3*)memalign(128, sizeof(vec3) * m_size)));
#else
    m_pdata.add_to_front(new pdata_arr(name, ptr));
#endif
}

vec3 *primitive::get_pdata_arr(const char* name)
{
    pdata_arr *n=static_cast<pdata_arr*>(m_pdata.m_head);
    while (n!=NULL)
    {
        if (!strcmp(name,n->m_name))
        {
            return n->m_array;
        }
        n=static_cast<pdata_arr*>(n->m_next);
    }
    return NULL;
}

void primitive::pdata_set(const char* name, int i, vec3 v)
{
    vec3* arr = get_pdata_arr(name);
    if (arr==NULL) return;
    arr[i]=v;
}

vec3 *primitive::pdata_get(const char* name, int i)
{
    vec3* arr = get_pdata_arr(name);
    return &arr[i];
}

void primitive::set_colour(flx_real r, flx_real g,
                           flx_real b, flx_real a)
{
    for (int i=0; i<m_size; i++)
    {
        m_colours[i].x = r;
        m_colours[i].y = g;
        m_colours[i].z = b;
    }
}

void primitive::apply(const mat44 &m)
{
    for (int i=0; i<m_size; i++)
    {
        m_positions[i]=m.transform(m_positions[i]);
//        m_normals[i]=m.transform_no_trans(m_normals[i]);
    }
}

list *primitive::intersect(const vec3 &start, const vec3 &end)
{
    return intersect_tristrip(start,end);

    switch (m_type)
    {
    case GL_TRIANGLE_STRIP: return intersect_tristrip(start,end);
    default: return NULL;
    }
}

bool primitive::intersect_fast(const vec3 &start, const vec3 &end)
{
    return intersect_tristrip_fast(start,end);
}

list *primitive::intersect_tristrip(const vec3 &start, const vec3 &end)
{
	vec3 bary;
	bool found=false;
	unsigned int i=2;
    list *points = new list;
	while(i<m_size)
	{
		unsigned int i1=i-2;
		unsigned int i2=i-1;
		unsigned int i3=i++;

        float t = intersect_line_triangle(start,end,*pdata_get("p",i1),
                                          *pdata_get("p",i2),
                                          *pdata_get("p",i3),bary);

		if (t>0)
		{
            points->add_to_front(interpolate_pdata(&m_pdata,t,bary,i1,i2,i3));
		}
	}
	return points;
}

bool primitive::intersect_tristrip_fast(const vec3 &start, const vec3 &end)
{
    vec3 bary;
	unsigned int i=2;
	while(i<m_size)
	{
		unsigned int i1=i-2;
		unsigned int i2=i-1;
		unsigned int i3=i++;

        flx_real t = intersect_line_triangle(start,end,*pdata_get("p",i1),
                                             *pdata_get("p",i2),
                                             *pdata_get("p",i3),bary);

		if (t>0.0)
		{
            return true;
		}
	}
	return false;
}

void primitive::render(u32 hints)
{
#ifdef _EE
    ps2_renderer::get()->render(0,m_size,&m_positions[0].x,&m_normals[0].x,&m_colours[0].x);
#else
#ifdef FLX_LINUX
    float *fltpos=new float[m_size*3];
    float *fltnrm=new float[m_size*3];
    float *fltcol=new float[m_size*3];
    float *flttex=new float[m_size*3];
    int pos=0;
    int cpos=0;
    for (int i=0; i<m_size; i++)
    {
        fltpos[pos]=m_positions[i].x;
        fltnrm[pos]=m_normals[i].x;
        fltcol[pos]=m_colours[i].x;
        flttex[pos++]=m_tex[i].x;
        fltpos[pos]=m_positions[i].y;
        fltnrm[pos]=m_normals[i].y;
        fltcol[pos]=m_colours[i].y;
        flttex[pos++]=m_tex[i].y;
        fltpos[pos]=m_positions[i].z;
        fltnrm[pos]=m_normals[i].z;
        fltcol[pos]=m_colours[i].z;
        flttex[pos++]=m_tex[i].z;
    }
    glVertexPointer(3, GL_FLOAT, 0, fltpos);
    glNormalPointer(GL_FLOAT, 0, fltnrm);
    glColorPointer(3, GL_FLOAT, 0, fltcol);
    glTexCoordPointer(3, GL_FLOAT, 0, flttex);
    glEnableClientState(GL_NORMAL_ARRAY);
    glEnableClientState(GL_COLOR_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);

    glColor3f(1,1,1);

    if (hints&HINT_SOLID)
    {
        glDrawArrays(m_type, 0, m_size);
    }

    if (hints&HINT_WIRE)
    {
        glDisableClientState(GL_COLOR_ARRAY);
        glColor3f(0,0,0);
        glDrawArrays(GL_LINE_STRIP, 0, m_size);
    }

    delete[] fltpos;
    delete[] fltnrm;
    delete[] fltcol;
    delete[] flttex;

#else

    glVertexPointer(3, GL_FIXED, 0, &m_positions[0]);

    if (m_colours!=NULL)
    {
        for (int i=0; i<m_size; i++)
        {
            m_colours_[i*4]=(float)m_colours[i].x*255.0f;
            m_colours_[i*4+1]=(float)m_colours[i].y*255.0f;
            m_colours_[i*4+2]=(float)m_colours[i].z*255.0f;
            m_colours_[i*4+3]=255;
        }

        glColorPointer(4, GL_UNSIGNED_BYTE, 0, m_colours_);
        glEnableClientState(GL_COLOR_ARRAY);
    }
    else
    {
        glDisableClientState(GL_COLOR_ARRAY);
    }

    if (m_normals!=NULL)
    {
        glNormalPointer(GL_FIXED, 0, &m_normals[0]);
        glEnableClientState(GL_NORMAL_ARRAY);
    }
    else
    {
        glDisableClientState(GL_NORMAL_ARRAY);
    }

    if (m_tex!=NULL)
    {
        glTexCoordPointer(3, GL_FIXED, 0, &m_tex[0]);
        glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    }
    else
    {
        glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    }

    if (hints&HINT_SOLID)
    {
        glDrawArrays(m_type, 0, m_size);
    }

    if (hints&HINT_WIRE)
    {
        glDisableClientState(GL_COLOR_ARRAY);
        glDrawArrays(GL_LINE_STRIP, 0, m_size);
    }
#endif
#endif // _EE
}
