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

#include <math.h>
#include <kernel.h>
#include <stdlib.h>

#include "ps2-renderer.h"

#include "app.h"
#include "engine/engine.h"
#include "scheme/scheme.h"

static long sStartTick = 0;
static long sTick = 0;
scheme *sc=NULL;;
FILE *log_file=NULL;

// Called from the app framework.
void appInit()
{
    engine::init();
    sc=scheme_init_new();    
    FILE *log_file=fopen("mass:nomadic/nomadic-log.txt","a+");
    if (log_file!=NULL) scheme_set_output_port_file(sc, log_file);
    else scheme_set_output_port_file(sc, stdout);
}

// Called from the app framework.
void appDeinit()
{
    fclose(log_file);
}

void appRender(long tick, int width, int height)
{
    if (sStartTick == 0)
        sStartTick = tick;
    if (!gAppAlive)
        return;

    // Actual tick value is "blurred" a little bit.
    sTick = (sTick + tick - sStartTick) >> 1;

    GS_SET_BGCOLOR(0x00,0x77,0xff);

    scheme_load_string(sc,"(frame-hook)");
        
    ps2_renderer::get()->start_frame();
    engine::get()->render();
    ps2_renderer::get()->end_frame();
}

void appEval(char *code)
{    
    if (code!=NULL)
    {
        GS_SET_BGCOLOR(0xff,0x77,0x22);
        scheme_load_string(sc,code);
        if (log_file!=NULL) fflush(log_file);
    }
}
