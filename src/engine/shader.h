// Copyright (C) 2005 Dave Griffiths
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

#ifndef FLX_GLSL_SHADER
#define FLX_GLSL_SHADER

#include <string>
#include <vector>
#include "core/mat44.h"
#include "core/vec3.h"

#include "importgl.h"

using namespace std;

//////////////////////////////////////////////////////
/// A pair of shaders, loaded and compiled -
/// needs to be made into a GLSLShader for use
class shader_pair
{
public:
	/// If load is true, the constructor attempts to load the shader pair immediately
	/// if it's false, the strings are treated as the shader source.
    shader_pair(bool load, const string &vertex, const string &fragment);
	~shader_pair();

	unsigned int get_vertex_shader() const { return m_vertex_shader; }
	unsigned int get_fragment_shader() const { return m_fragment_shader; }

private:
	/// Returns a handle to a compiled and linked GLSL program
	bool load(const string &vertex_filename, const string &fragment_filename);
	bool make(const string &vertex_source, const string &fragment_source);
	unsigned int load_shader(string filename, unsigned int type);
	unsigned int make_shader(const string &filename, const string &source, unsigned int type);

	unsigned int m_vertex_shader;
	unsigned int m_fragment_shader;
};

//////////////////////////////////////////////////////
/// A hardware shader for use on an object
class shader
{
public:
	/// The constructor attempts to load the shader pair immediately
	shader() : m_ref_count(1), m_is_valid(false) {}
	shader(const shader_pair &pair);
	~shader();

	// Temp fix, maybe
	void inc_ref() { m_ref_count++; }
	bool dec_ref() { m_ref_count--; return (m_ref_count==0); }

	/////////////////////////////////////////////
	///@name Renderer interface
	///@{
	static void init();
	void apply();
	static void unapply();
	bool is_valid() { return m_is_valid; }
	///@}

	/////////////////////////////////////////////
	///@name Uniform variables
	///@{
	void set_int(const string &name, int s);
	void set_float(const string &name, float s);
	void set_vector(const string &name, vec3 s);
	void set_int_array(const string &name, const vector<int> &s);
	void set_float_array(const string &name, const vector<float> &s);
	void set_matrix(const string &name, mat44 &m);
	void set_vector_array(const string &name, const vector<vec3> &s);
	///@}

	/////////////////////////////////////////////
	///@name Attribute variables
	///@{
	void set_float_attrib(const string &name, const vector<float> &s);
	void set_vector_attrib(const string &name, const vec3 *s);
	///@}

	static bool m_enabled;

private:
	unsigned int m_program;
	unsigned int m_ref_count;
	bool m_is_valid;
};


#endif
