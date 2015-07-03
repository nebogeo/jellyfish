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

#include <stdlib.h>
#include <string>
#include "core/types.h"
#include "scheme/scheme.h"

class interpreter {
public:
    ~interpreter();
    static void initialise();
    static void eval(const std::string &code);
    static void eval_file(const std::string &filename);
    static std::string load_file(const std::string &filename);

    static void start_repl(pthread_mutex_t *render_mutex);

private:
    static void repl_loop();
    static scheme *m_sc;
    static FILE *m_log_file;
    static pthread_mutex_t *m_render_mutex;
};
