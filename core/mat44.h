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
#include <string.h>
#include "vec3.h"

#ifndef FLX_MAT44
#define FLX_MAT44

static const flx_real TWO_PI=3.141592654*2.0f;
static const flx_real DEG_CONV = 0.017453292f;
static const flx_real RAD_CONV = 1/0.017453292f;

class mat44
{
public:
    mat44() { init(); }
    mat44(const mat44 &other) { (*this)=other; }

	mat44(flx_real m00, flx_real m10, flx_real m20, flx_real m30, 
					flx_real m01, flx_real m11, flx_real m21, flx_real m31, 
					flx_real m02, flx_real m12, flx_real m22, flx_real m32, 
					flx_real m03, flx_real m13, flx_real m23, flx_real m33)
	{
		m[0][0]=m00; m[1][0]=m10; m[2][0]=m20; m[3][0]=m30;
		m[0][1]=m01; m[1][1]=m11; m[2][1]=m21; m[3][1]=m31;
		m[0][2]=m02; m[1][2]=m12; m[2][2]=m22; m[3][2]=m32;
		m[0][3]=m03; m[1][3]=m13; m[2][3]=m23; m[3][3]=m33;
	}
	
	inline void init()
	{
		zero();
		m[0][0]=m[1][1]=m[2][2]=m[3][3]=1.0f;
	}

	inline void zero()
	{
		memset(m,0,sizeof(flx_real)*16);
	}

	inline const mat44 &operator=(mat44 const &rhs)
	{
		m[0][0]=rhs.m[0][0]; m[0][1]=rhs.m[0][1]; m[0][2]=rhs.m[0][2]; m[0][3]=rhs.m[0][3];
		m[1][0]=rhs.m[1][0]; m[1][1]=rhs.m[1][1]; m[1][2]=rhs.m[1][2]; m[1][3]=rhs.m[1][3];
		m[2][0]=rhs.m[2][0]; m[2][1]=rhs.m[2][1]; m[2][2]=rhs.m[2][2]; m[2][3]=rhs.m[2][3];
		m[3][0]=rhs.m[3][0]; m[3][1]=rhs.m[3][1]; m[3][2]=rhs.m[3][2]; m[3][3]=rhs.m[3][3];
		return rhs;
	}

	inline mat44 operator+(mat44 const &rhs) const
	{
    	mat44 t;
    	for (u32 i=0; i<4; i++)
		{
        	for (u32 j=0; j<4; j++)
			{
            	t.m[i][j]=m[i][j]+rhs.m[i][j];
			}
		}
    	return t;
	}

	inline mat44 operator-(mat44 const &rhs) const
	{
    	mat44 t;
    	for (u32 i=0; i<4; i++)
		{
        	for (u32 j=0; j<4; j++)
			{
            	t.m[i][j]=m[i][j]-rhs.m[i][j];
			}
		}
    	return t;
	}

	inline mat44 operator*(mat44 const &rhs) const
	{
        mat44 t;
    	t.m[0][0]=m[0][0]*rhs.m[0][0]+m[1][0]*rhs.m[0][1]+m[2][0]*rhs.m[0][2]+m[3][0]*rhs.m[0][3];
    	t.m[0][1]=m[0][1]*rhs.m[0][0]+m[1][1]*rhs.m[0][1]+m[2][1]*rhs.m[0][2]+m[3][1]*rhs.m[0][3];
    	t.m[0][2]=m[0][2]*rhs.m[0][0]+m[1][2]*rhs.m[0][1]+m[2][2]*rhs.m[0][2]+m[3][2]*rhs.m[0][3];
    	t.m[0][3]=m[0][3]*rhs.m[0][0]+m[1][3]*rhs.m[0][1]+m[2][3]*rhs.m[0][2]+m[3][3]*rhs.m[0][3];

    	t.m[1][0]=m[0][0]*rhs.m[1][0]+m[1][0]*rhs.m[1][1]+m[2][0]*rhs.m[1][2]+m[3][0]*rhs.m[1][3];
    	t.m[1][1]=m[0][1]*rhs.m[1][0]+m[1][1]*rhs.m[1][1]+m[2][1]*rhs.m[1][2]+m[3][1]*rhs.m[1][3];
    	t.m[1][2]=m[0][2]*rhs.m[1][0]+m[1][2]*rhs.m[1][1]+m[2][2]*rhs.m[1][2]+m[3][2]*rhs.m[1][3];
    	t.m[1][3]=m[0][3]*rhs.m[1][0]+m[1][3]*rhs.m[1][1]+m[2][3]*rhs.m[1][2]+m[3][3]*rhs.m[1][3];

    	t.m[2][0]=m[0][0]*rhs.m[2][0]+m[1][0]*rhs.m[2][1]+m[2][0]*rhs.m[2][2]+m[3][0]*rhs.m[2][3];
    	t.m[2][1]=m[0][1]*rhs.m[2][0]+m[1][1]*rhs.m[2][1]+m[2][1]*rhs.m[2][2]+m[3][1]*rhs.m[2][3];
    	t.m[2][2]=m[0][2]*rhs.m[2][0]+m[1][2]*rhs.m[2][1]+m[2][2]*rhs.m[2][2]+m[3][2]*rhs.m[2][3];
    	t.m[2][3]=m[0][3]*rhs.m[2][0]+m[1][3]*rhs.m[2][1]+m[2][3]*rhs.m[2][2]+m[3][3]*rhs.m[2][3];

    	t.m[3][0]=m[0][0]*rhs.m[3][0]+m[1][0]*rhs.m[3][1]+m[2][0]*rhs.m[3][2]+m[3][0]*rhs.m[3][3];
    	t.m[3][1]=m[0][1]*rhs.m[3][0]+m[1][1]*rhs.m[3][1]+m[2][1]*rhs.m[3][2]+m[3][1]*rhs.m[3][3];
    	t.m[3][2]=m[0][2]*rhs.m[3][0]+m[1][2]*rhs.m[3][1]+m[2][2]*rhs.m[3][2]+m[3][2]*rhs.m[3][3];
    	t.m[3][3]=m[0][3]*rhs.m[3][0]+m[1][3]*rhs.m[3][1]+m[2][3]*rhs.m[3][2]+m[3][3]*rhs.m[3][3];

    	return t;
	}

	inline mat44 operator/(mat44 const &rhs) const
	{
    	mat44 t;
    	for (u32 i=0; i<4; i++)
		{
        	for (u32 j=0; j<4; j++)
			{
            	t.m[i][j]=m[i][0]/rhs.m[0][j]+
                    	  m[i][1]/rhs.m[1][j]+
                    	  m[i][2]/rhs.m[2][j]+
                    	  m[i][3]/rhs.m[3][j];
			}
		}
    	return t;
	}

	inline mat44 operator+(flx_real rhs) const
	{
		mat44 t;
    	for (u32 i=0; i<4; i++)
		{
        	for (u32 j=0; j<4; j++)
			{
            	t.m[i][j]=m[i][j]+rhs;
			}
		}
    	return t;
	}

	inline mat44 operator-(flx_real rhs) const
	{
		mat44 t;
    	for (u32 i=0; i<4; i++)
		{
        	for (u32 j=0; j<4; j++)
			{
            	t.m[i][j]=m[i][j]-rhs;
			}
		}
    	return t;
	}

	inline mat44 operator*(flx_real rhs) const
	{
		mat44 t;
    	for (u32 i=0; i<4; i++)
		{
        	for (u32 j=0; j<4; j++)
			{
            	t.m[i][j]=m[i][j]*rhs;
			}
		}
    	return t;
	}

	inline mat44 operator/(flx_real rhs) const
	{
		mat44 t;
    	for (u32 i=0; i<4; i++)
		{
        	for (u32 j=0; j<4; j++)
			{
            	t.m[i][j]=m[i][j]/rhs;
			}
		}
    	return t;
	}

	inline mat44 &operator+=(mat44 const &rhs)
	{
    	for (u32 i=0; i<4; i++)
		{
        	for (u32 j=0; j<4; j++)
			{
            	m[i][j]+=rhs.m[i][j];
			}
		}
    	return *this;
	}

	inline mat44 &operator-=(mat44 const &rhs)
	{
    	for (u32 i=0; i<4; i++)
		{
        	for (u32 j=0; j<4; j++)
			{
            	m[i][j]-=rhs.m[i][j];
			}
		}
    	return *this;
	}

	inline mat44 &operator*=(mat44 const &rhs)
	{
    	*this=*this*rhs;
    	return *this;
	}

	inline mat44 &operator/=(mat44 const &rhs)
	{
    	*this=*this/rhs;
    	return *this;
	}

	inline mat44 &translate(flx_real x, flx_real y, flx_real z)
	{
    	mat44 t;
    	t.m[3][0]=x;
    	t.m[3][1]=y;
    	t.m[3][2]=z;
    	*this=*this*t;
    	return *this;

	}

	inline mat44 &translate(vec3 &tr)
	{
    	mat44 t;
    	t.m[3][0]=tr.x;
    	t.m[3][1]=tr.y;
    	t.m[3][2]=tr.z;
    	*this=*this*t;
    	return *this;

	}

	inline void settranslate(const vec3 &tr)
	{
    	m[3][0]=tr.x;
    	m[3][1]=tr.y;
    	m[3][2]=tr.z;
	}

	inline vec3 gettranslate() const
	{
    	return vec3(m[3][0],m[3][1],m[3][2]);
	}

	//#define USE_FAST_SINCOS

	inline mat44 &rotxyz(flx_real x,flx_real y,flx_real z)
	{
		mat44 t;
		if (x!=0.0f)
		{
			x*=0.017453292f;

			#ifdef USE_FAST_SINCOS
			flx_real sx,cx;
			dSinCos(x,sx,cx);
			#else
			flx_real sx=sin(x);
			flx_real cx=cos(x);
			#endif

    		t.m[1][1]=cx;
    		t.m[2][1]=-sx;
			t.m[1][2]=sx;
			t.m[2][2]=cx;
			*this=*this*t;
		}

		if (y!=0.0f)
		{
			y*=0.017453292f;

			#ifdef USE_FAST_SINCOS
			flx_real sy,cy;
			dSinCos(y,sy,cy);
			#else
			flx_real sy=sin(y);
			flx_real cy=cos(y);
			#endif

			t.init();
			t.m[0][0]=cy;
	    	t.m[2][0]=sy;
			t.m[0][2]=-sy;
			t.m[2][2]=cy;
			*this=*this*t;
    	}

    	if (z!=0.0f)
    	{
    		z*=0.017453292f;

    		#ifdef USE_FAST_SINCOS
    		flx_real sz,cz;
    		dSinCos(z,sz,cz);
    		#else
			flx_real sz=sin(z);
			flx_real cz=cos(z);
			#endif

			t.init();
    		t.m[0][0]=cz;
    		t.m[1][0]=-sz;
			t.m[0][1]=sz;
			t.m[1][1]=cz;
	    	*this=*this*t;
    	}

    	return *this;
	}

	inline mat44 &rotx(flx_real a)
	{
    	a*=0.017453292f;
    	mat44 t;

    	t.m[1][1]=cos(a);
    	t.m[2][1]=-sin(a);
		t.m[1][2]=sin(a);
		t.m[2][2]=cos(a);

    	*this=*this*t;
    	return *this;
	}

	inline mat44 &roty(flx_real a)
	{
    	a*=0.017453292f;
    	mat44 t;

    	t.m[0][0]=cos(a);
    	t.m[2][0]=-sin(a);
        t.m[0][2]=sin(a);
        t.m[2][2]=cos(a);

    	*this=*this*t;
    	return *this;
	}

	inline mat44 &rotz(flx_real a)
	{
    	a*=0.017453292f;
    	mat44 t;

    	t.m[0][0]=cos(a);
    	t.m[1][0]=-sin(a);
		t.m[0][1]=sin(a);
		t.m[1][1]=cos(a);

    	*this=*this*t;
    	return *this;
	}

	inline mat44 &scale(flx_real x, flx_real y, flx_real z)
	{
    	mat44 t;

    	t.m[0][0]=x;
    	t.m[1][1]=y;
		t.m[2][2]=z;

    	*this=*this*t;
    	return *this;
	}

    inline mat44& scale(const vec3& s)
    {
        return scale(s.x, s.y, s.z);
    }

	inline vec3 transform(vec3 const &p) const
	{
    	vec3 t;
    	t.x=p.x*m[0][0] + p.y*m[1][0] + p.z*m[2][0] + m[3][0];
    	t.y=p.x*m[0][1] + p.y*m[1][1] + p.z*m[2][1] + m[3][1]; 
        t.z=p.x*m[0][2] + p.y*m[1][2] + p.z*m[2][2] + m[3][2];
    	return t;
	}

	inline vec3 transform_persp(vec3 const &p) const
	{
    	vec3 t;
    	t.x=p.x*m[0][0] + p.y*m[1][0] + p.z*m[2][0];
    	t.y=p.x*m[0][1] + p.y*m[1][1] + p.z*m[2][1];
    	t.z=p.x*m[0][2] + p.y*m[1][2] + p.z*m[2][2];
    	return t;
	}

	inline vec3 transform_no_trans(vec3 const &p) const
	{
    	vec3 t;
    	t.x=p.x*m[0][0] + p.y*m[1][0] + p.z*m[2][0];
    	t.y=p.x*m[0][1] + p.y*m[1][1] + p.z*m[2][1];
    	t.z=p.x*m[0][2] + p.y*m[1][2] + p.z*m[2][2];
    	return t;
	}

	/*void load_glmatrix(flx_real glm[16])
	{
		glm[0]= m[0][0]; glm[1]= m[1][0]; glm[2]= m[2][0]; glm[3]= m[3][0];
		glm[4]= m[0][1]; glm[5]= m[1][1]; glm[6]= m[2][1]; glm[7]= m[3][1];
		glm[8]= m[0][2]; glm[9]= m[1][2]; glm[10]=m[2][2]; glm[11]=m[3][2];
		glm[12]=m[0][3]; glm[13]=m[1][3]; glm[14]=m[2][3]; glm[15]=m[3][3];
	}*/

	inline void load_glmatrix(flx_real glm[16])
	{
		glm[0]= m[0][0]; glm[4]= m[1][0]; glm[8]= m[2][0]; glm[12]= m[3][0];
		glm[1]= m[0][1]; glm[5]= m[1][1]; glm[9]= m[2][1]; glm[13]= m[3][1];
		glm[2]= m[0][2]; glm[6]= m[1][2]; glm[10]=m[2][2]; glm[14]=m[3][2];
		glm[3]= m[0][3]; glm[7]= m[1][3]; glm[11]=m[2][3]; glm[15]=m[3][3];
	}

	inline void load_mat44(flx_real glm[16])
	{
		m[0][0]=glm[0]; m[1][0]=glm[4]; m[2][0]=glm[8]; m[3][0]=glm[12];
		m[0][1]=glm[1]; m[1][1]=glm[5]; m[2][1]=glm[9]; m[3][1]=glm[13];
		m[0][2]=glm[2]; m[1][2]=glm[6]; m[2][2]=glm[10]; m[3][2]=glm[14];
		m[0][3]=glm[3]; m[1][3]=glm[7]; m[2][3]=glm[11]; m[3][3]=glm[15];
	}

	inline mat44 getTranspose() const
	{
		mat44 t;
		for (u32 i=0; i<4; i++)
		{
        	for (u32 j=0; j<4; j++)
			{
            	t.m[i][j]=m[j][i];
			}
		}
    	return t;
	}

    inline void transpose()
    {
        *this = getTranspose();
    }

	inline mat44 inverse() const
	{
		mat44 temp;
		temp.m[0][0] = m[1][2]*m[2][3]*m[3][1] - m[1][3]*m[2][2]*m[3][1] + m[1][3]*m[2][1]*m[3][2] - m[1][1]*m[2][3]*m[3][2] - m[1][2]*m[2][1]*m[3][3] + m[1][1]*m[2][2]*m[3][3];
		temp.m[0][1] = m[0][3]*m[2][2]*m[3][1] - m[0][2]*m[2][3]*m[3][1] - m[0][3]*m[2][1]*m[3][2] + m[0][1]*m[2][3]*m[3][2] + m[0][2]*m[2][1]*m[3][3] - m[0][1]*m[2][2]*m[3][3];
		temp.m[0][2] = m[0][2]*m[1][3]*m[3][1] - m[0][3]*m[1][2]*m[3][1] + m[0][3]*m[1][1]*m[3][2] - m[0][1]*m[1][3]*m[3][2] - m[0][2]*m[1][1]*m[3][3] + m[0][1]*m[1][2]*m[3][3];
		temp.m[0][3] = m[0][3]*m[1][2]*m[2][1] - m[0][2]*m[1][3]*m[2][1] - m[0][3]*m[1][1]*m[2][2] + m[0][1]*m[1][3]*m[2][2] + m[0][2]*m[1][1]*m[2][3] - m[0][1]*m[1][2]*m[2][3];
		temp.m[1][0] = m[1][3]*m[2][2]*m[3][0] - m[1][2]*m[2][3]*m[3][0] - m[1][3]*m[2][0]*m[3][2] + m[1][0]*m[2][3]*m[3][2] + m[1][2]*m[2][0]*m[3][3] - m[1][0]*m[2][2]*m[3][3];
		temp.m[1][1] = m[0][2]*m[2][3]*m[3][0] - m[0][3]*m[2][2]*m[3][0] + m[0][3]*m[2][0]*m[3][2] - m[0][0]*m[2][3]*m[3][2] - m[0][2]*m[2][0]*m[3][3] + m[0][0]*m[2][2]*m[3][3];
		temp.m[1][2] = m[0][3]*m[1][2]*m[3][0] - m[0][2]*m[1][3]*m[3][0] - m[0][3]*m[1][0]*m[3][2] + m[0][0]*m[1][3]*m[3][2] + m[0][2]*m[1][0]*m[3][3] - m[0][0]*m[1][2]*m[3][3];
		temp.m[1][3] = m[0][2]*m[1][3]*m[2][0] - m[0][3]*m[1][2]*m[2][0] + m[0][3]*m[1][0]*m[2][2] - m[0][0]*m[1][3]*m[2][2] - m[0][2]*m[1][0]*m[2][3] + m[0][0]*m[1][2]*m[2][3];
		temp.m[2][0] = m[1][1]*m[2][3]*m[3][0] - m[1][3]*m[2][1]*m[3][0] + m[1][3]*m[2][0]*m[3][1] - m[1][0]*m[2][3]*m[3][1] - m[1][1]*m[2][0]*m[3][3] + m[1][0]*m[2][1]*m[3][3];
		temp.m[2][1] = m[0][3]*m[2][1]*m[3][0] - m[0][1]*m[2][3]*m[3][0] - m[0][3]*m[2][0]*m[3][1] + m[0][0]*m[2][3]*m[3][1] + m[0][1]*m[2][0]*m[3][3] - m[0][0]*m[2][1]*m[3][3];
		temp.m[2][2] = m[0][1]*m[1][3]*m[3][0] - m[0][3]*m[1][1]*m[3][0] + m[0][3]*m[1][0]*m[3][1] - m[0][0]*m[1][3]*m[3][1] - m[0][1]*m[1][0]*m[3][3] + m[0][0]*m[1][1]*m[3][3];
		temp.m[2][3] = m[0][3]*m[1][1]*m[2][0] - m[0][1]*m[1][3]*m[2][0] - m[0][3]*m[1][0]*m[2][1] + m[0][0]*m[1][3]*m[2][1] + m[0][1]*m[1][0]*m[2][3] - m[0][0]*m[1][1]*m[2][3];
		temp.m[3][0] = m[1][2]*m[2][1]*m[3][0] - m[1][1]*m[2][2]*m[3][0] - m[1][2]*m[2][0]*m[3][1] + m[1][0]*m[2][2]*m[3][1] + m[1][1]*m[2][0]*m[3][2] - m[1][0]*m[2][1]*m[3][2];
		temp.m[3][1] = m[0][1]*m[2][2]*m[3][0] - m[0][2]*m[2][1]*m[3][0] + m[0][2]*m[2][0]*m[3][1] - m[0][0]*m[2][2]*m[3][1] - m[0][1]*m[2][0]*m[3][2] + m[0][0]*m[2][1]*m[3][2];
		temp.m[3][2] = m[0][2]*m[1][1]*m[3][0] - m[0][1]*m[1][2]*m[3][0] - m[0][2]*m[1][0]*m[3][1] + m[0][0]*m[1][2]*m[3][1] + m[0][1]*m[1][0]*m[3][2] - m[0][0]*m[1][1]*m[3][2];
		temp.m[3][3] = m[0][1]*m[1][2]*m[2][0] - m[0][2]*m[1][1]*m[2][0] + m[0][2]*m[1][0]*m[2][1] - m[0][0]*m[1][2]*m[2][1] - m[0][1]*m[1][0]*m[2][2] + m[0][0]*m[1][1]*m[2][2];
        flx_real d=temp.determinant();
        if (d!=0.f)
        {
            flx_real scale=1.0f/d;
            temp.scale(scale,scale,scale);
        }
        else temp.scale(0,0,0);
        return temp;
	}

	inline flx_real determinant()  const
	{
	   return 
	   m[0][3] * m[1][2] * m[2][1] * m[3][0]-m[0][2] * m[1][3] * m[2][1] * m[3][0]-m[0][3] * m[1][1] * m[2][2] * m[3][0]+m[0][1] * m[1][3] * m[2][2] * m[3][0]+
	   m[0][2] * m[1][1] * m[2][3] * m[3][0]-m[0][1] * m[1][2] * m[2][3] * m[3][0]-m[0][3] * m[1][2] * m[2][0] * m[3][1]+m[0][2] * m[1][3] * m[2][0] * m[3][1]+
	   m[0][3] * m[1][0] * m[2][2] * m[3][1]-m[0][0] * m[1][3] * m[2][2] * m[3][1]-m[0][2] * m[1][0] * m[2][3] * m[3][1]+m[0][0] * m[1][2] * m[2][3] * m[3][1]+
	   m[0][3] * m[1][1] * m[2][0] * m[3][2]-m[0][1] * m[1][3] * m[2][0] * m[3][2]-m[0][3] * m[1][0] * m[2][1] * m[3][2]+m[0][0] * m[1][3] * m[2][1] * m[3][2]+
	   m[0][1] * m[1][0] * m[2][3] * m[3][2]-m[0][0] * m[1][1] * m[2][3] * m[3][2]-m[0][2] * m[1][1] * m[2][0] * m[3][3]+m[0][1] * m[1][2] * m[2][0] * m[3][3]+
	   m[0][2] * m[1][0] * m[2][1] * m[3][3]-m[0][0] * m[1][2] * m[2][1] * m[3][3]-m[0][1] * m[1][0] * m[2][2] * m[3][3]+m[0][0] * m[1][1] * m[2][2] * m[3][3];
	}

	inline void aim(vec3 v, const vec3& up)
	{
		v.normalise();
		vec3 l=v.cross(up);
		vec3 u=v.cross(l);
		l.normalise();
		u.normalise();

		m[0][0]=v.x; m[0][1]=v.y; m[0][2]=v.z;
		m[1][0]=l.x; m[1][1]=l.y; m[1][2]=l.z;
		m[2][0]=u.x; m[2][1]=u.y; m[2][2]=u.z;	
	}

	inline void blend(const mat44& other, flx_real amount)
	{
		for (u32 j=0; j<4; j++)
		{
        	for (u32 i=0; i<4; i++)
			{
            	m[i][j]=(1.0f-amount)*m[i][j]+amount*other.m[i][j];
			}
		}
	}

    inline flx_real *arr()
    {
        return &m[0][0];
    }
    
#ifdef _EE 
    flx_real m[4][4] __attribute__((__aligned__(16)));
#else
    flx_real m[4][4];
#endif
};

#endif
