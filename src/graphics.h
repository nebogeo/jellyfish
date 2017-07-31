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

#include <string>
#include "engine/importgl.h"

#ifdef FLX_RPI
#include "rpi/graphics.h"
#endif

class graphics {
public:

    #ifdef FLX_RPI
    static RPI_STATE_T *rpi_state;
    #endif

    static pthread_mutex_t* m_render_mutex;

    static void initialise();
    static void init_mutex();

    // in case we don't have glu/glut
    static void do_perspective(GLfloat fovy, GLfloat aspect, GLfloat zNear, GLfloat zFar);
    static void prepare_frame(int width, int height);
    static void configure_light_and_material();
    static void render(int width, int height);

    static u32 load_texture_from_file(const std::string &path, const std::string &filename);
    static u32 load_texture_raw(const std::string &texname,int width, int height, char* data);

    // glut callbacks
    static void display_callback();
    static void keyboard_callback(unsigned char key,int x, int y);
    static void keyboard_up_callback(unsigned char key,int x, int y);

#ifdef FLX_LINUX
    // no glut on pi
    static void reshape_callback(int width, int height);
    static void idle_callback();
    static void mouse_callback(int button, int state, int x, int y);
    static void motion_callback(int x, int y);
    static void passive_motion_callback(int x, int y);
#endif

    static const std::string INPUT_CALLBACK;
    static const std::string INPUT_RELEASE_CALLBACK;

    static int m_w;
    static int m_h;
    static bool m_record;

private:

    static int m_frame_num;
};
