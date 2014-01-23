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

#include "ps2-renderer.h"
#include <stdio.h>

extern u32 vu1_unlit_CodeStart __attribute__((section(".vudata")));
extern u32 vu1_unlit_CodeEnd __attribute__((section(".vudata")));

ps2_renderer* ps2_renderer::m_renderer=NULL;

ps2_renderer::ps2_renderer()
{
    m_env=pdkScreenSetup();
    pdkVu1UploadProg(0,&vu1_unlit_CodeStart,&vu1_unlit_CodeEnd);

    pdkMatrixViewScreen( &m_view_screen, 512.0f,1.33f,1.0f, 
                         2048+640/2,2048+256/2,
                         1.0f, 6777215.0f,64.0f, 5536.0f );
    
    // Setup the matrix for our camera
    
    pdkMatrixIdentity(&m_world_view);
    pdkMatrixTranslate(&m_world_view, 0, 0, 20);
}

ps2_renderer::~ps2_renderer()
{
}

void ps2_renderer::set_camera(float *m)
{
//    pdkMatrixCopyRaw(&m_world_view,m);
}


void ps2_renderer::start_frame()
{
    m_tx_stack.clear();
    m_tx_stack.add_to_front(new tx);
    pdkScreenClear(m_env);
    pdkMatrixMultiply(&m_world_screen, &m_world_view, &m_view_screen);
}

void ps2_renderer::render(u32 type, u32 vertex_count, float *p, float *n, float *c)
{
    GS_SET_BGCOLOR(0xFF,0x00,0x00);

    pdkVu1ListBegin();
    pdkMatrix matrix;
    tx *top=static_cast<tx*>(m_tx_stack.m_head);
    pdkMatrixMultiply(&matrix, &top->m_tx, &m_world_screen);

    // upload matrix first  
    pdkVu1ListData(0,static_cast<void*>(&matrix),4);

    // then parameters
    u32 params[4];
    params[0]=vertex_count;
    params[1]=0;
    params[2]=0;
    params[3]=0;
    pdkVu1ListData(4,static_cast<void*>(params),1);

    // now start the gifpacket
    pdkVu1ListAddBegin(5);
    pdkVu1ListAdd128(
        GS_GIFTAG(vertex_count, 1, 1, 
                  GS_PRIM( GS_PRIM_TRIANGLE, GS_PRIM_SGOURAUD, GS_PRIM_TOFF, 
                           GS_PRIM_FOFF, GS_PRIM_ABOFF, GS_PRIM_AAOFF, GS_PRIM_FSTQ, 
                           GS_PRIM_C1, 0), 
                  GS_GIFTAG_PACKED, 2),
        (((u64)GS_REG_RGBAQ) <<  0 |
         ((u64)GS_REG_XYZ2)  <<  4));

    // now the primitive data (interleaved)
    int ii=0;
    int cc=0;
    for (int i=0; i<vertex_count; i++)
    {
        pdkVu1ListAdd32(c[cc++]*255);
        pdkVu1ListAdd32(c[cc++]*255);    
        pdkVu1ListAdd32(c[cc++]*255);
        pdkVu1ListAdd32(0); 
        cc++;

        pdkVu1ListAddFloat(p[ii++]);
        pdkVu1ListAddFloat(p[ii++]);
        pdkVu1ListAddFloat(p[ii++]);
        pdkVu1ListAddFloat(1);
        ii++;
    }
    pdkVu1ListAddEnd();

    // end the list and start the vu program (located in micromem adress 0)
    
    pdkVu1ListEnd(0);
    GS_SET_BGCOLOR(0x00,0xff,0x00);
}

void ps2_renderer::end_frame()
{    
    GS_SET_BGCOLOR(0x00,0xff,0xff);
    pdkScreenVsync(m_env);
    pdkScreenFlip(m_env);
}

void ps2_renderer::mult_matrix(float *m)
{
    tx *top=static_cast<tx*>(m_tx_stack.m_head);
    pdkMatrix mat;
    pdkMatrixCopyRaw(&mat,m);
    pdkMatrixMultiply(&top->m_tx,&mat,&top->m_tx);
}

void ps2_renderer::push_matrix()
{
    if (m_tx_stack.m_head!=NULL)
    {
        tx *top=static_cast<tx*>(m_tx_stack.m_head);
        m_tx_stack.add_to_front(new tx(*top));
    }
}

void ps2_renderer::pop_matrix()
{
    if (m_tx_stack.size()>1)
    {
        delete m_tx_stack.remove_from_front();
    }
}
