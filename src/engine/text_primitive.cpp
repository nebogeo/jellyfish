// Copyright (C) 2005 Dave Griffiths
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

#include "text_primitive.h"

text_primitive::text_primitive(u32 max_chars, flx_real charw, flx_real charh, int charstride, int wrapchars) :
    primitive(max_chars*6,TRIANGLES),
    m_max_chars(max_chars),
    m_char_width(charw),
    m_char_height(charh),
    m_char_stride(charstride),
    m_wrap_chars(wrapchars),
    m_xoff(0),
    m_yoff(0),
    m_crowd(0.6)
{
}

void text_primitive::set_text(const char *str)
{
    unsigned int length=strlen(str);
	flx_real x=0,y=0;
	flx_real w=m_char_width*20.0f;
    flx_real h=m_char_height*-20.0f;

    //20,-20,0.018
	m_text_width=m_char_width*(float)length;
	m_text_height=m_char_height;
	int wrapcount=0;	
    u32 vp=0;
    flx_real z=0;
    flx_real zm=0.0001;

	for (u32 n=0; n<m_size; n++)
	{
        m_positions[n].x=0;
        m_positions[n].y=0;
        m_positions[n].z=0;
    }

	for (u32 n=0; n<length; n++)
	{
        if (n<m_max_chars)
        {
            u32 ascii_pos=(u32)str[n];   
            flx_real s=(float)(ascii_pos%m_char_stride)*m_char_width+m_xoff;
            flx_real t=(float)(ascii_pos/m_char_stride)*m_char_height+m_yoff;
            vec3 min(s,t,0);
            vec3 max(s+m_char_width,t+m_char_height,0);

            m_positions[vp].x=x;
            m_positions[vp].y=y;
            m_positions[vp].z=z;
            m_tex[vp].x=min.x;
            m_tex[vp].y=min.y;
            vp++;

            m_positions[vp].x=x+w;
            m_positions[vp].y=y;
            m_positions[vp].z=z;
            m_tex[vp].x=max.x;
            m_tex[vp].y=min.y;
            vp++;
            
            m_positions[vp].x=x+w;
            m_positions[vp].y=y+h;
            m_positions[vp].z=z;
            m_tex[vp].x=max.x;
            m_tex[vp].y=max.y;
            vp++;

            //---

            m_positions[vp].x=x;
            m_positions[vp].y=y;
            m_positions[vp].z=z;
            m_tex[vp].x=min.x;
            m_tex[vp].y=min.y;
            vp++;

            m_positions[vp].x=x+w;
            m_positions[vp].y=y+h;
            m_positions[vp].z=z;
            m_tex[vp].x=max.x;
            m_tex[vp].y=max.y;
            vp++;

            m_positions[vp].x=x;
            m_positions[vp].y=y+h;
            m_positions[vp].z=z;
            m_tex[vp].x=min.x;
            m_tex[vp].y=max.y;
            vp++;

            z+=zm;
            
            if (m_wrap_chars) wrapcount++;
            
            if (str[n]=='\n' || (m_wrap_chars && wrapcount>m_wrap_chars))
            {
                y+=h;
                m_text_height+=h;
                x=0;
                wrapcount=0;
            }
            else
            {
                x+=(w - m_crowd);
            }
        }
	}
}

void text_primitive::set_text_params(flx_real w, flx_real h, int stride, int wrap, flx_real xoff, flx_real yoff, flx_real crowd)
{
	m_char_width=w;
	m_char_height=h;
	m_char_stride=stride;
	m_wrap_chars=wrap;
	m_xoff=xoff;
	m_yoff=yoff;
	m_crowd=crowd;
}

void text_primitive::render(u32 hints)
{
	primitive::render(hints);
}


