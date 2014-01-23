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

ps2_renderer* ps2_renderer::m_renderer=NULL;

ps2_renderer::ps2_renderer()
{
	// The data packets for double buffering dma sends.
	packet_allocate(&m_packet[0],4096,0,0);
	packet_allocate(&m_packet[1],4096,0,0);

	// Init GIF dma channel.
	dma_channel_initialize(DMA_CHANNEL_GIF,NULL,0);
	dma_channel_fast_waits(DMA_CHANNEL_GIF);

	// Define a 32-bit 640x512 framebuffer.
	m_frame.width = 640;
	m_frame.height = 512;
	m_frame.mask = 0;
	m_frame.psm = GS_PSM_32;
	m_frame.address = graph_vram_allocate(m_frame.width,m_frame.height, m_frame.psm, GRAPH_ALIGN_PAGE);

	// Enable the zbuffer.
	m_z.enable = DRAW_ENABLE;
	m_z.mask = 0;
	m_z.method = ZTEST_METHOD_GREATER_EQUAL;
	m_z.zsm = GS_ZBUF_32;
	m_z.address = graph_vram_allocate(m_frame.width,m_frame.height,m_z.zsm, GRAPH_ALIGN_PAGE);

	// Initialize the screen and tie the first framebuffer to the read circuits.
	graph_initialize(m_frame.address,m_frame.width,m_frame.height,m_frame.psm,0,0);

	// This is our generic qword pointer.
	QWORD *q = m_packet->data;

	// This will setup a default drawing environment.
	q = draw_setup_environment(q,0,&m_frame,&m_z);

	// Now reset the primitive origin to 2048-width/2,2048-height/2.
	q = draw_primitive_xyoffset(q,0,(2048-320),(2048-256));

	// Finish setting up the environment.
	q = draw_finish(q);

	// Now send the packet, no need to wait since it's the first.
	dma_channel_send_normal(DMA_CHANNEL_GIF,m_packet->data,q - m_packet->data, 0, 0);

	// Define the triangle primitive we want to use.
    m_prim.type = PRIM_TRIANGLE_STRIP;
	m_prim.shading = PRIM_SHADE_GOURAUD;
	m_prim.mapping = DRAW_DISABLE;
	m_prim.fogging = DRAW_DISABLE;
	m_prim.blending = DRAW_DISABLE;
	m_prim.antialiasing = DRAW_DISABLE;
	m_prim.mapping_type = PRIM_MAP_ST;
	m_prim.colorfix = PRIM_UNFIXED;

    // Create the view_screen matrix.
	create_view_screen(m_view_screen, graph_aspect_ratio(), -3.00f, 3.00f, -3.00f, 3.00f, 1.00f, 2000.00f);

	// Wait for any previous dma transfers to finish before starting.
	dma_wait_fast();

    m_context=0;
    matrix_unit(m_world_view);
}

ps2_renderer::~ps2_renderer()
{
}

void ps2_renderer::set_camera(float *m)
{
    matrix_copy(m_world_view,m);
}


void ps2_renderer::start_frame()
{
    m_tx_stack.clear();
    m_tx_stack.add_to_front(new tx);

	QWORD *dmatag;

    // Grab our dmatag pointer for the dma chain.
    m_dmatag = m_packet[m_context].data;
    // Now grab our qword pointer and increment past the dmatag.
    m_q = m_dmatag;
    m_q++;
    
    // Clear framebuffer but don't update zbuffer.
    m_q = draw_disable_tests(m_q,0,&m_z);
    m_q = draw_clear(m_q,0,2048.0f-320.0f,2048.0f-256.0f,m_frame.width,m_frame.height,0x00,0x00,0x00);
    m_q = draw_enable_tests(m_q,0,&m_z);

	DMATAG_CNT(m_dmatag,m_q-m_dmatag-1,0,0,0);
}

void ps2_renderer::render(u32 type, u32 vertex_count, float *p, float *n)
{
    GS_SET_BGCOLOR(0xFF,0x00,0x00);

	COLOR color;
	color.r = 0xff;
	color.g = 0x66;
	color.b = 0x33;
	color.a = 0xff;
	color.q = 1.0f;

///
    // Create the local_screen matrix.
    tx *top=static_cast<tx*>(m_tx_stack.m_head);
    create_local_screen(m_local_screen, top->m_tx, m_world_view, m_view_screen);
    MATRIX light;
    // copy transform
    for (int i=0; i<16; i++) light[i]=top->m_tx[i];
    // strip away translation
    light[11]=0;
    light[12]=0;
    light[13]=0;

    GS_SET_BGCOLOR(0x00,0x88,0xff);

    VECTOR *temp_vertices=(VECTOR*)memalign(128, sizeof(VECTOR) * vertex_count);
	XYZ *verts=(XYZ*)memalign(128, sizeof(VERTEX) * vertex_count);
    calculate_vertices(temp_vertices, vertex_count, (float (*)[4])p, m_local_screen);
    // Convert floating point vertices to fixed point and translate to center of screen.
    draw_convert_xyz(verts, 2048, 2048, 32, vertex_count, (VERTEXF*)temp_vertices);

    VECTOR *temp_normals=(VECTOR*)memalign(128, sizeof(VECTOR) * vertex_count);
    XYZ *normals=(XYZ*)memalign(128, sizeof(VERTEX) * vertex_count);
    calculate_normals(temp_normals, vertex_count, (float(*)[4])n, light); 
  
    // Convert floating point colours to fixed point.
    //draw_convert_rgbq(colors, vertex_count, (VERTEXF*)temp_vertices, (COLORF*)colours, color.a);
   
    // Draw the triangles using triangle primitive type.
    m_q = draw_prim_start(m_q,0,&m_prim, &color);
    
    VECTOR light_dir={0.0f,1.0f,0.0f};
    GS_SET_BGCOLOR(0xff,0x00,0xff);

    for(int i = 0; i < vertex_count; i++)
    {
        GS_SET_BGCOLOR((char)i*10,0x00,0x00);

        // stupid diffuse lighting
        VECTOR nn;
        vector_normalize(nn, temp_normals[i]);
        float l=vector_innerproduct(nn,light_dir);
        if (l<0.0f) l=0.0f; 
        if (l>1.0f) l=1.0f;
        COLOR c;
        c.r=temp_normals[i][0]*128.0f;
        c.g=temp_normals[i][1]*128.0f;
        c.b=temp_normals[i][2]*128.0f;
        c.a=0xff;
        c.q=1.0f;
        
        m_q->dw[0] = c.rgbaq;
        m_q->dw[1] = verts[i].xyz;
        m_q++;
    }
    GS_SET_BGCOLOR(0xff,0x00,0x00);
    
    free(verts);
    free(temp_vertices);
    free(normals);
    free(temp_normals);

    m_q = draw_prim_end(m_q,2,DRAW_RGBAQ_REGLIST);

	DMATAG_CNT(m_dmatag,m_q-m_dmatag-1,0,0,0);
}

void ps2_renderer::end_frame()
{    
    // Setup a finish event.
    m_q = draw_finish(m_q);
    
    // Define our dmatag for the dma chain.
    DMATAG_END(m_dmatag,m_q-m_packet[m_context].data-1,0,0,0);

    // Now send our current dma chain.
    dma_wait_fast();
    dma_channel_send_chain(DMA_CHANNEL_GIF,m_packet[m_context].data, m_q - m_packet[m_context].data, 0, 0);
    
    GS_SET_BGCOLOR(0x00,0x00,0x00);

    // Now switch our packets so we can process data while the DMAC is working.
    m_context ^= 1;

    graph_wait_vsync();

    // Wait for scene to finish drawing
    draw_wait_finish();    
}

void ps2_renderer::mult_matrix(float *m)
{
    tx *top=static_cast<tx*>(m_tx_stack.m_head);
    matrix_multiply(top->m_tx,m,top->m_tx);
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
