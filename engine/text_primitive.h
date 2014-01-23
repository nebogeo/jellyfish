// Copyright (C) 2012 Dave Griffiths
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

#ifndef FLX_TEXTPRIMITIVE
#define FLX_TEXTPRIMITIVE

#include "core/fixed.h"
#include "primitive.h"

// A dodgy font drawing primitive, uses texture
// mapping and quads to form words.
class text_primitive : public primitive
{
public:
	/// charw,h are in _texture_ coords not pixels
	text_primitive(u32 max_chars, flx_real charw, flx_real charh, int charstride, int wrapchars=0);
	~text_primitive() {}
	
	virtual void render(u32 hints);

	void set_text(const char *s);
	flx_real get_text_width() { return m_text_width; }
	flx_real get_text_height() { return m_text_height; }
	
	void set_text_params(flx_real w, flx_real h, int stride, int wrap, flx_real xoff, flx_real yoff, flx_real crowd);
	
protected:
	
    u32 m_max_chars;
	flx_real m_char_width;
	flx_real m_char_height;
	u32 m_char_stride;
	flx_real m_text_width;
	flx_real m_text_height;
	u32 m_wrap_chars;
	flx_real m_xoff;
	flx_real m_yoff;
	flx_real m_crowd;	
};

#endif
