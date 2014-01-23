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

#include "types.h"
#include "stdio.h"

#ifndef FLX_VEC3
#define FLX_VEC3

class vec3
{
public:
    vec3()
    {
        x=y=z=0.0f;
#ifdef _EE
        _ = 0.0f;
#endif
    }

    vec3(flx_real X, flx_real Y, flx_real Z) { x=X; y=Y; z=Z; }
    vec3(vec3 const &c) { *this=c; }

    bool operator==(vec3 const &rhs) { return (x==rhs.x&&y==rhs.y&&z==rhs.z); }

    inline vec3 &operator=(vec3 const &rhs)
    {
        x=rhs.x; y=rhs.y; z=rhs.z;
        return *this;
    }

    inline vec3 operator+(vec3 const &rhs) const
    {
        vec3 t;
        t.x=x+rhs.x; t.y=y+rhs.y; t.z=z+rhs.z;
        return t;
    }

    inline vec3 operator-(vec3 const &rhs) const
    {
        vec3 t;
        t.x=x-rhs.x; t.y=y-rhs.y; t.z=z-rhs.z;
        return t;
    }

    inline vec3 operator*(vec3 const &rhs) const
    {
        vec3 t;
        t.x=x*rhs.x;
        t.y=y*rhs.y;
        t.z=z*rhs.z;
        return t;
    }

    inline vec3 operator/(vec3 const &rhs) const
    {
        vec3 t;
        t.x=x/rhs.x;
        t.y=y/rhs.y;
        t.z=z/rhs.z;
        return t;
    }

    inline vec3 operator+(flx_real rhs) const
    {
        vec3 t;
        t.x=x+rhs; t.y=y+rhs; t.z=z+rhs;
        return t;
    }

    inline vec3 operator-(flx_real rhs) const
    {
        vec3 t;
        t.x=x-rhs; t.y=y-rhs; t.z=z-rhs;
        return t;
    }

    inline vec3 operator*(flx_real rhs) const
    {
        vec3 t;
        t.x=x*rhs; t.y=y*rhs; t.z=z*rhs;
        return t;
    }

    inline vec3 operator/(flx_real rhs) const
    {
        vec3 t;
        t.x=x/rhs; t.y=y/rhs; t.z=z/rhs;
        return t;
    }

    inline vec3 &operator+=(vec3 const &rhs)
    {
        x+=rhs.x; y+=rhs.y; z+=rhs.z;
        return *this;
    }

    inline vec3 &operator-=(vec3 const &rhs)
    {
        x-=rhs.x; y-=rhs.y; z-=rhs.z;
        return *this;
    }

    inline vec3 &operator*=(flx_real rhs)
    {
        x*=rhs; y*=rhs; z*=rhs;
        return *this;
    }

    inline vec3 &operator/=(flx_real rhs)
    {
        if (rhs!=0.0f) {x/=rhs; y/=rhs; z/=rhs;}
        return *this;
    }

    inline flx_real dot(vec3 const &rhs) const
    {
        return x*rhs.x+y*rhs.y+z*rhs.z;
    }

    inline vec3 cross(vec3 const &rhs) const
    {
        return vec3(y*rhs.z - z*rhs.y,
                    z*rhs.x - x*rhs.z,
                    x*rhs.y - y*rhs.x);
    }

    inline vec3 reflect(vec3 const &rhs) const
    {
        flx_real vdn=dot(rhs)*2.0f;
        return (*this)-rhs*vdn;
    }

    inline flx_real dist(vec3 const &rhs) const
    {
        return sqrt((float)((rhs.x-x)*(rhs.x-x)+
                            (rhs.y-y)*(rhs.y-y)+
                            (rhs.z-z)*(rhs.z-z)));
    }

    inline flx_real distsq(vec3 const &rhs) const
    {
        return (rhs.x-x)*(rhs.x-x)+
            (rhs.y-y)*(rhs.y-y)+
            (rhs.z-z)*(rhs.z-z);
    }

    inline flx_real mag()
    {
        // tendancy to blow up (overflow) in fixed...
        return sqrt((float)x*(float)x+(float)y*(float)y+(float)z*(float)z);
    }

    inline flx_real magsq()
    {
        float xx=x;
        float yy=y;
        float zz=z;
        return ((xx*xx)+(yy*yy)+(zz*zz));
    }

    inline bool feq(const vec3 &other, flx_real epsilon=0.001)
    {
        return (fabs((float)(x-other.x))<epsilon &&
                fabs((float)(y-other.y))<epsilon &&
                fabs((float)(z-other.z))<epsilon);
    }

    inline bool operator<(vec3 rhs)
    {
			return x<rhs.x && y<rhs.y && z<rhs.z;
    }

    inline bool operator>(vec3 rhs)
    {
        return x>rhs.x && y>rhs.y && z>rhs.z;
    }

    inline vec3 &normalise() { *this/=mag(); return *this; }

    char *as_str()
    {
        char *str=new char[256];
        snprintf(str,256,"%f %f %f",(float)x,(float)y,(float)z);
        return str;
    }

#ifdef _EE
    flx_real x,y,z,_;// __attribute__((__aligned__(16)));
#else
    flx_real x,y,z;
#endif

private:
};

#endif
