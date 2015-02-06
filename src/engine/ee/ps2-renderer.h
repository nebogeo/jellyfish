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

#include <kernel.h>
#include <stdlib.h>
#include <tamtypes.h>

#include "pdk/screen.h"
#include "pdk/matrix.h"
#include "pdk/vu1.h"
#include "pdk/output.h"
#include "pdk/gs.h"

#include "../../core/list.h"

#define gs_p_bgcolor		0x120000e0	// Set CRTC background color
#define GS_SET_BGCOLOR(r,g,b) \
			*(volatile unsigned long *)gs_p_bgcolor =	\
		(unsigned long)((r) & 0x000000FF) <<  0 | \
		(unsigned long)((g) & 0x000000FF) <<  8 | \
		(unsigned long)((b) & 0x000000FF) << 16

class ps2_renderer
{
public:
    static void init() { m_renderer=new ps2_renderer; }
    static ps2_renderer *get() { return m_renderer; }

    void start_frame();
    void render(u32 type, u32 vertex_count, float *p, float *n, float *c);
    void end_frame();
    void push_matrix();
    void pop_matrix();
    void mult_matrix(float *);

    void set_camera(float *);

    pdkGsEnv* m_env; 

private:
    ps2_renderer();
    ~ps2_renderer();

    pdkMatrix m_local_world;
    pdkMatrix m_world_view;
    pdkMatrix m_view_screen;
    pdkMatrix m_local_screen;
    pdkMatrix m_world_screen;

    static ps2_renderer *m_renderer;

    class tx : public list::node
    {
    public:
        tx() { pdkMatrixIdentity(&m_tx); }
        tx(tx &other) { pdkMatrixCopy(&m_tx,&other.m_tx); }
        pdkMatrix m_tx;
    };

    list m_tx_stack;

};
