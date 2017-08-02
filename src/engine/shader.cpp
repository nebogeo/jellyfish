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

#include <stdio.h>
#include <iostream>
#include <assert.h>

#include "engine/shader.h"

using namespace std;

#ifdef FLX_RPI // using ES1.0 = no shaders
#define NO_SHADERS
#endif

bool shader::m_enabled(true);

shader_pair::shader_pair(bool do_load, const string &vertex, const string &fragment) :
  m_vertex_shader(0),
  m_fragment_shader(0) {
  if (do_load) {
    if (!load(vertex, fragment)) {
      cerr<<"Problem loading shaderpair ["<<vertex<<", "<<fragment<<"]"<<endl;
    }
  } else {
    if (!make(vertex, fragment)) {
      cerr<<"Problem making shaderpair"<<endl;
    }
  }
}

shader_pair::~shader_pair() {
#ifndef NO_SHADERS
  if (!shader::m_enabled) {
    if (m_vertex_shader!=0) glDeleteShader(m_vertex_shader);
    if (m_fragment_shader!=0) glDeleteShader(m_fragment_shader);
  }
#endif
}

bool shader_pair::make(const string &vertexsource, const string &fragmentsource) {
  if (!shader::m_enabled) return true;
#ifndef NO_SHADERS

  if (vertexsource.empty()) {
  m_vertex_shader = 0;
} else {
  m_vertex_shader = make_shader("Inline vertex shader source",vertexsource,GL_VERTEX_SHADER);
  if (m_vertex_shader==0) return false;
}

  if (fragmentsource.empty()) {
  m_fragment_shader = 0;
} else {
  m_fragment_shader = make_shader("Inline fragment shader source",fragmentsource,GL_FRAGMENT_SHADER);
  if (m_fragment_shader==0) return false;
}

  if (!m_vertex_shader && !m_fragment_shader) {
  cerr << "No shaders specifed" << endl;
  return false;
}
#endif
  return true;
}

bool shader_pair::load(const string &vertexfilename, const string &fragmentfilename) {
  if (!shader::m_enabled) return true;
#ifndef NO_SHADERS
  m_vertex_shader = 0;
  m_fragment_shader = 0;
  m_vertex_shader = load_shader(vertexfilename,GL_VERTEX_SHADER);
  if (m_vertex_shader == 0) return false;
  m_fragment_shader = load_shader(fragmentfilename,GL_FRAGMENT_SHADER);
  if (m_fragment_shader == 0) return false;
#endif
  return true;
}


  unsigned int shader_pair::load_shader(string filename, unsigned int type) {
  if (!shader::m_enabled) return 0;
#ifndef NO_SHADERS
  FILE* file = fopen(filename.c_str(), "r");
  if (!file) {
    cerr<<"Couldn't open shader ["<<filename<<"]"<<endl;
    return 0;
  }

  fseek(file, 0, SEEK_END);
  unsigned int size = ftell(file);
  fseek(file, 0, SEEK_SET);

  char* code = new char[size+1];
  code[size]='\0';

  if (fread(code,1,size,file)!=size) {
    cerr<<"Error reading shader ["<<filename<<"]"<<endl;
    delete[] code;
    fclose(file);
    return 0;
  } else {
    unsigned int shader = make_shader(filename,code,type);
    delete[] code;
    fclose(file);
    return shader;
  }
#endif
  return 0;
}


unsigned int shader_pair::make_shader(const string &filename, const string &source, unsigned int type) {
  if (!shader::m_enabled) return 0;
#ifndef NO_SHADERS
  unsigned int shader = glCreateShader(type);
  const char *t = source.c_str();
  glShaderSource(shader, 1, &t, NULL);

  glCompileShader(shader);

  GLint status = GL_FALSE;
  glGetShaderiv(shader, GL_COMPILE_STATUS, &status);
  if(status != GL_TRUE) {
  GLsizei size = 0;
  char log[1024];

  glGetShaderInfoLog(shader, 1024, &size, log);
  cerr<<"compile errors for ["<<filename<<"]"<<endl;
  cerr<<log<<endl;

  glDeleteShader(shader);
  return 0;
}
  return shader;
#endif
  return 0;
}



  /////////////////////////////////////////

  shader::shader(const shader_pair &pair) :
    m_program(0),
    m_ref_count(1)
      {
  if (!m_enabled) return;
#ifndef NO_SHADERS
  
  m_program = glCreateProgram();
  if (pair.get_vertex_shader())
    glAttachShader(m_program, pair.get_vertex_shader());
  if (pair.get_fragment_shader())
    glAttachShader(m_program, pair.get_fragment_shader());
  glLinkProgram(m_program);

  GLint status = GL_FALSE;
  glGetProgramiv(m_program, GL_LINK_STATUS, &status);
  if (status != GL_TRUE) {
    char log[1024];
    glGetProgramInfoLog(m_program, 1024, NULL, log);
    cerr<<log<<endl;
  }

  glValidateProgram(m_program);
  glGetProgramiv(m_program, GL_VALIDATE_STATUS, &status);
  if (status != GL_TRUE) {
    char log[1024];
    glGetProgramInfoLog(m_program, 1024, NULL, log);
    cerr<<log<<endl;
  } else {
    m_is_valid = true;
  }
#endif
}

shader::~shader() {
  if (!m_enabled) return;
#ifndef NO_SHADERS
  glDeleteProgram(m_program);
#endif
}

  void shader::init() {
  if(glewInit() != GLEW_OK) {
  cerr<< "ERROR Unable to check OpenGL extensions" << endl;
}
  m_enabled = glewIsSupported("GL_VERSION_2_0");
}

  void shader::apply() {
  if (!m_enabled) return;
#ifndef NO_SHADERS
  glUseProgram(m_program);
#endif
}

void shader::unapply() {
  if (!m_enabled) return;
#ifndef NO_SHADERS
  glUseProgram(0);
#endif
}

void shader::set_int(const string &name, int s) {
  if (!m_enabled) return;
#ifndef NO_SHADERS
  GLuint param = glGetUniformLocation(m_program, name.c_str());
  glUniform1i(param,s);
#endif
}

void shader::set_float(const string &name, float s) {
  if (!m_enabled) return;
#ifndef NO_SHADERS
  GLuint param = glGetUniformLocation(m_program, name.c_str());
  glUniform1f(param,s);
#endif
}

void shader::set_vector(const string &name, vec3 s) {
  if (!m_enabled) return;
#ifndef NO_SHADERS
  GLuint param = glGetUniformLocation(m_program, name.c_str());
  glUniform3f(param, s.x, s.y, s.z);
#endif
}

void shader::set_matrix(const string &name, mat44 &m) {
  if (!m_enabled) return;
#ifndef NO_SHADERS
  GLuint param = glGetUniformLocation(m_program, name.c_str());
  glUniformMatrix4fv(param, 1, GL_FALSE, &m.m[0][0]);
#endif
}

void shader::set_int_array(const string &name, const vector<int> &s) {
  if (!m_enabled) return;
#ifndef NO_SHADERS
  GLuint param = glGetUniformLocation(m_program, name.c_str());
  glUniform1iv(param,s.size(),&(*s.begin()));
#endif
}

void shader::set_float_array(const string &name, const vector<float> &s) {
  if (!m_enabled) return;
#ifndef NO_SHADERS
  GLuint param = glGetUniformLocation(m_program, name.c_str());
  glUniform1fv(param,s.size(),&(*s.begin()));
#endif
}

void shader::set_vector_array(const string &name, const vector<vec3> &s) {
  if (!m_enabled) return;
#ifndef NO_SHADERS
  GLuint param = glGetUniformLocation(m_program, name.c_str());
  glUniform4fv(param,s.size(),&s.begin()->x);
#endif
}

void shader::set_float_attrib(const string &name, const vector<float> &s) {
  if (!m_enabled) return;
#ifndef NO_SHADERS
  GLuint attrib = glGetAttribLocation(m_program, name.c_str());
  glEnableVertexAttribArray(attrib);
  glVertexAttribPointer(attrib,1,GL_FLOAT,false,0,&(*s.begin()));
#endif
}

void shader::set_vector_attrib(const string &name, const vec3 *s) {
  if (!m_enabled) return;
#ifndef NO_SHADERS
  GLuint attrib = glGetAttribLocation(m_program, name.c_str());
  glEnableVertexAttribArray(attrib);
  glVertexAttribPointer(attrib,3,GL_FLOAT,false,0,s);
#endif
}
