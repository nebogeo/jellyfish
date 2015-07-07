// Copyright (C) 2015 Dave Griffiths
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

#include <pthread.h>
#include <stdio.h>
#include <app.h>
#include "core/osc.h"
#include "interpreter.h"
#include "graphics.h"

pthread_mutex_t *network_osc::m_render_mutex=NULL;

void network_osc::osc_error_handler(int num, const char *msg, const char *path)
{
    printf("liblo server error %d in path %s\n",num,path);
}

int network_osc::osc_default_handler(const char *path, const char *types, lo_arg **argv, int argc, void *data, void *user_data)
{
    //printf("osc server no handler for: %s %s\n",path,types);
	return 1;
}

int network_osc::osc_eval_handler(const char *path, const char *types, lo_arg **argv,
		    int argc, void *data, void *user_data)
{
    if (types[0]=='s') {
        printf("%s\n",argv[0]);
        pthread_mutex_lock(m_render_mutex);
        interpreter::eval((char*)argv[0]);
        pthread_mutex_unlock(m_render_mutex);
    }
	return 1;
}

void network_osc::start_osc_repl(pthread_mutex_t* render_mutex) {
    m_render_mutex = render_mutex;
    printf("starting osc, listening to port 8000\n");
	lo_server_thread server = lo_server_thread_new("8000", osc_error_handler);
    lo_server_thread_add_method(server, NULL, NULL, osc_default_handler, NULL);
    lo_server_thread_add_method(server, "/eval", "s", osc_eval_handler, NULL);
	lo_server_thread_start(server);
}
