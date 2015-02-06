// Copyright (C) 2012 Dave Griffiths
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

#ifndef FLX_MSG
#define FLX_MSG

#ifdef FLX_LINUX
//-Linux--------------------------------

inline void msg(const char *str)
{
    printf("%s\n",str);
}

#else
#ifdef _EE
//-PS2----------------------------------


#else
#ifdef FLX_RPI

inline void msg(const char *str)
{
    printf("%s\n",str);
}

#else
//-Android------------------------------



#include <android/log.h>

inline void msg(const char *str)
{
    __android_log_write(ANDROID_LOG_ERROR,"starwisp",str);
}

#endif
#endif
#endif

inline void msg_vec(vec3 v)
{
    char *s=v.as_str();
    msg(s);
    delete[] s;
}

#endif // FLX_TYPES
