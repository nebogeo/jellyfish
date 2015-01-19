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

#ifndef JFISH_PRIM
#define JFISH_PRIM

#include "engine/primitive.h"

class jellyfish;

class jellyfish_primitive : public primitive
{
public:
    jellyfish_primitive(u32 size);
    ~jellyfish_primitive();

    virtual void build();
    virtual void execute();
    virtual void render(u32 hints);

private:
    jellyfish *m_machine;
    mat44 m_internal_tx;
};

#endif
