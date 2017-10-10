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
#include <lo/lo.h>

/////////////// osc stuff ////////////////////////////////////////////////

class network_osc {
public:
    static void start_osc_repl(pthread_mutex_t *render_mutex);
    static void send(const char *url, const char *name, const char *types, char *data, size_t size);

private:
    static void osc_error_handler(int num, const char *msg, const char *path);
    static int osc_default_handler(const char *path, const char *types, lo_arg **argv, int argc, void *data, void *user_data);
    static int osc_eval_handler(const char *path, const char *types, lo_arg **argv, int argc, void *data, void *user_data);
    static int osc_sync_handler(const char *path, const char *types, lo_arg **argv, int argc, void *data, void *user_data);
    static int osc_delay_handler(const char *path, const char *types, lo_arg **argv, int argc, void *data, void *user_data);
    static pthread_mutex_t *m_render_mutex;

    static float m_sync_delay;
};
