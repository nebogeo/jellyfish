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

#include <string.h>
#include "list.h"
#include "vec3.h"

#ifndef FLX_PDATA
#define FLX_PDATA

class pdata_arr : public list::node
{
public:
    pdata_arr(const char* n, vec3 *a) : m_name(strdup(n)), m_array(a) {}
    ~pdata_arr() { free(m_name); delete[] m_array; }
    
    char *m_name;
    vec3 *m_array; 
};

#endif
