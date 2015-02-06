// Copyright (C) 2008 Gabor Papp
//
// Perlin Noise based on the code by Karsten Schmidt
// http://toxiclibs.googlecode.com/svn/trunk/toxiclibs/src.geom/toxi/math/noise/PerlinNoise.java
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

class Noise
{
	public:
		static void noise_detail(int octaves, float falloff = 0);
		static void noise_seed(unsigned seed);

		static float noise(float x, float y = 0, float z = 0);

	private:

		static const int PERLIN_YWRAPB = 4;
		static const int PERLIN_YWRAP = 1 << PERLIN_YWRAPB;
		static const int PERLIN_ZWRAPB = 8;
		static const int PERLIN_ZWRAP = 1 << PERLIN_ZWRAPB;
		static const int PERLIN_SIZE = PERLIN_YWRAP * PERLIN_ZWRAP;
		static const float PERLIN_MIN_AMPLITUDE;

		static bool inited;
		static float perlin[4096];
		static int perlin_octaves;
		static float perlin_amp_falloff;
		static unsigned perlin_seed;
};


