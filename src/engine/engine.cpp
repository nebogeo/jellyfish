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

#include "engine.h"
#include "text_primitive.h"
#include "jellyfish_primitive.h"
#include "instance_primitive.h"
#include "obj_reader.h"
#include "shader.h"

#ifdef _EE
#include "ee/ps2-renderer.h"
#endif

engine *engine::m_engine=NULL;

// add new state items here
engine::state_stack_item::state_stack_item() {
  m_colr=m_colg=m_colb=m_cola=1.0f;
  m_hints=HINT_SOLID;
  m_line_width=1;
  m_texture=0;
  m_shader=NULL;
  m_srcblend=GL_SRC_ALPHA;
  m_dstblend=GL_ONE_MINUS_SRC_ALPHA;
}

engine::state_stack_item::state_stack_item(const state_stack_item &other) {
  m_tx=other.m_tx;
  m_parent=other.m_parent;
  m_colr=other.m_colr;
  m_colg=other.m_colg;
  m_colb=other.m_colb;
  m_cola=other.m_cola;
  m_hints=other.m_hints;
  m_line_width=other.m_line_width;
  m_texture=other.m_texture;
  m_shader=NULL; // eh?
  m_srcblend=other.m_srcblend;
  m_dstblend=other.m_dstblend;
}

engine::engine() {
  m_sg=new scenegraph();
  clear();
  m_attached_prim = NULL;
  m_attached_id = 0;
}

engine::~engine() {
}

void engine::init() {
  if (m_engine==NULL) {
    m_engine=new engine();
  }
}

engine *engine::get() {
  return m_engine;
}

engine::state_stack_item *engine::state_top() {
  return static_cast<state_stack_item*>(m_state_stack.m_head);
}

void engine::push() {
  state_stack_item *si = new state_stack_item(*state_top());
  m_state_stack.add_to_front(si);
}

void engine::pop() {
  // always leave one
  if (m_state_stack.m_head!=NULL &&
      m_state_stack.m_head->m_next!=NULL) {
    delete m_state_stack.remove_from_front();
  }
}

void engine::grab(int id) {
  grab_stack_item *si = new grab_stack_item();
  si->m_id=id;
  m_grab_stack.add_to_front(si);
}

void engine::ungrab() {
  if (m_grab_stack.m_head!=NULL) {
    delete m_grab_stack.remove_from_front();
  }
}

bool engine::grabbed() {
  return m_grab_stack.m_head!=NULL &&
    grabbed_node()!=NULL;
}

int engine::grabbed_id() {
  return static_cast<grab_stack_item*>
    (m_grab_stack.m_head)->m_id;
}

scenenode *engine::grabbed_node() {
  scenenode *n=m_sg->find(grabbed_id());
  if (n==NULL)
    {
      //        cerr<<"could not find node "<<grabbed_id()<<endl;
    }
  return n;
}

void engine::lock_camera(int id) {
  m_attached_id=id;
  m_attached_prim=m_sg->find(id);
}

void engine::identity() {
  if (grabbed()) {
    grabbed_node()->m_tx.init();
  } else {
    state_top()->m_tx.init();
  }
}

void engine::translate(float x, float y, float z) {
  if (grabbed()) {
    grabbed_node()->m_tx.translate(x,y,z);
  } else {
    state_top()->m_tx.translate(x,y,z);
  }
}

void engine::scale(float x, float y, float z) {
  if (grabbed()) {
    grabbed_node()->m_tx.scale(x,y,z);
  } else {
    state_top()->m_tx.scale(x,y,z);
  }
}

void engine::rotate(float x, float y, float z) {
  if (grabbed()) {
    grabbed_node()->m_tx.rotxyz(x,y,z);
  } else {
    state_top()->m_tx.rotxyz(x,y,z);
  }
}

void engine::concat(mat44 m) {
  if (grabbed()) {
    grabbed_node()->m_tx*=m;
  } else {
    state_top()->m_tx*=m;
  }
}

void engine::aim(float x, float y, float z, float ux, float uy, float uz) {
  if (grabbed()) {
    grabbed_node()->m_tx.aim(vec3(x,y,z),vec3(ux,uy,uz));
  } else {
    state_top()->m_tx.aim(vec3(x,y,z),vec3(ux,uy,uz));
  }  
}


void engine::colour(float r, float g, float b, float a) {
  if (grabbed()) {
    scenenode *n = grabbed_node();
    if (n && n->m_primitive!=NULL) {
      n->m_primitive->set_colour(r,g,b,a);
    }
  } else {
    state_top()->m_colr=r;
    state_top()->m_colg=g;
    state_top()->m_colb=b;
    state_top()->m_cola=a;
  }
}

void engine::blend_mode(u32 src, u32 dst) {
  if (grabbed()) {
    scenenode *n = grabbed_node();
    if (n) {
      n->m_srcblend=src;
      n->m_dstblend=dst;
    }
  } else {
    state_top()->m_srcblend=src;
    state_top()->m_dstblend=dst;
  }
}

void engine::hint(u32 hint) {
  if (grabbed()) {
    scenenode *n = grabbed_node();
    if (n) {
      if (hint==0) n->m_hints=0;
      else n->m_hints|=hint;
    }
  } else {
    if (hint==0) state_top()->m_hints=0;
    else state_top()->m_hints|=hint;
  }
}

void engine::line_width(u32 w) {
  if (grabbed()) {
    scenenode *n = grabbed_node();
    if (n) n->m_line_width=w;
  } else {
    state_top()->m_line_width=w;
  }
}

void engine::texture(u32 id) {
  if (grabbed()) {
    scenenode *n = grabbed_node();
    if (n) n->m_texture=id;
  } else {
    state_top()->m_texture=id;
  }
}


void engine::set_shader(const string &vertex,
			const string &fragment) {
  if (grabbed()) {
    scenenode *n = grabbed_node();
    if (n) {
      if (n->m_shader!=NULL) delete n->m_shader;
      n->m_shader = new shader(shader_pair(false,vertex,fragment));
    }
  } else {
    if (state_top()->m_shader!=NULL) delete state_top()->m_shader;
    state_top()->m_shader = new shader(shader_pair(false,vertex,fragment));
  }
}

shader *engine::get_current_shader() {
  if (grabbed()) {
    scenenode *n = grabbed_node();
    if (n) return n->m_shader;
  } else {
    return state_top()->m_shader;
  }
}

void engine::parent(int p) {
  if (grabbed()) {
    scenenode *n = grabbed_node();
    if (n)
      {
	state_top()->m_parent=p;
	m_sg->reparent(n->m_id,p);
      }
  } else {
    state_top()->m_parent=p;
  }
}

// add new state items here...
void engine::setup_state(scenenode *n) {
  n->m_tx=state_top()->m_tx;
  n->m_primitive->set_colour(state_top()->m_colr,
			     state_top()->m_colg,
			     state_top()->m_colb,
			     state_top()->m_cola);
  n->m_hints=state_top()->m_hints;
  n->m_line_width=state_top()->m_line_width;
  n->m_texture=state_top()->m_texture;
  n->m_srcblend=state_top()->m_srcblend;
  n->m_dstblend=state_top()->m_dstblend;
}

void engine::draw_instance(int id) {
  scenenode *node = m_sg->find(id);
  if (node) {
    // copy the primitive only
    // (set to null after rendering so it don't get deleted)
    // perhaps allocate a bunch of these at startup?
    scenenode *n=new scenenode(node->m_primitive);
    setup_state(n);
    m_sg->add_immediate(n);
  }
}


int engine::build_cube() {
  scenenode *n=new scenenode(new primitive());
  setup_state(n);
  return m_sg->add(state_top()->m_parent,n);
}

int engine::raw_obj(char *data) {
  obj_reader reader;
  reader.RawRead(data);
  return build_obj(reader);
}

int engine::load_obj(char *fn) {
  obj_reader reader;
  reader.FormatRead(fn);
  return build_obj(reader);
}

int engine::build_obj(obj_reader &reader) {
  primitive *p = new primitive(reader.m_Indices.size(),primitive::TRIANGLES);
  p->build();

  if (reader.m_Faces[0].Index.size()!=3) {
    printf("non triangular faces, trouble...\n");
  }

  // copy stuff from reader to primitive
  // super slow but should only happen at load time?
  for (int i=0; i<p->pdata_size(); ++i) {
    p->pdata_set("p",i,reader.m_Position[reader.m_Indices[i]]);
  }

  if (reader.m_Normal.size()>0) {
    for (int i=0; i<p->pdata_size(); ++i) {
      p->pdata_set("n",i,reader.m_Normal[reader.m_Indices[i]]);
    }
  }

  if (reader.m_Texture.size()>0) {
    for (int i=0; i<p->pdata_size(); ++i) {
      p->pdata_set("t",i,reader.m_Texture[reader.m_Indices[i]]);
    }
  }

  scenenode *n=new scenenode(p);
  setup_state(n);
  return m_sg->add(state_top()->m_parent,n);
}

int engine::build_text(char *str) {
  // 16*16 grid of letters
  text_primitive *p = new text_primitive(64,16/256.0f,16/256.0f,16,0);
  scenenode *n=new scenenode(p);
  p->set_text(str,20,-20,0);
  setup_state(n);
  return m_sg->add(state_top()->m_parent,n);
}

int engine::build_jellyfish(u32 size) {
  scenenode *n=new scenenode(new jellyfish_primitive(size));
  setup_state(n);
  return m_sg->add(state_top()->m_parent,n);
}

int engine::build_instance(u32 id) {
  scenenode *srcn=m_sg->find(id);
  if (srcn==NULL) {
    printf("cannot find instance source");
    return -1;
  }
  scenenode *n=new scenenode(new instance_primitive(srcn->m_primitive));
  setup_state(n);
  return m_sg->add(state_top()->m_parent,n);
}


int engine::build_polygons(unsigned int size, int type) {
  scenenode *n=new scenenode(new primitive(size,static_cast<primitive::type>(type)));
  setup_state(n);
  return m_sg->add(state_top()->m_parent,n);
}

void engine::clear() {
  m_state_stack.clear();
  state_stack_item *si = new state_stack_item();
  m_state_stack.add_to_front(si);
  m_sg->clear();
  m_camera_tx.init();
  m_camera_tx.translate(0,0,-10);
  m_clear_r=m_clear_g=m_clear_b=0.0f;
  m_clear_a=1.0f;
}

void engine::destroy(int id) {
  // clear camera attachment if it's this primitive
  if (m_attached_id == id) {
    m_attached_prim = NULL;
    m_attached_id = 0;
  }

  m_sg->remove(id);
}


void engine::text_set(const char *str) {
  if (grabbed()) {
    scenenode *n = grabbed_node();
    if (n && n->m_primitive!=NULL) {
      // todo: check
      ((text_primitive*)(n->m_primitive))->set_text(str,20,-20,0);
    }
  }
}
//                             0              1           2      3            4             5               6              7               8               9                10
void engine::text_params(char *text, flx_real w, flx_real h, int stride, int wrap, flx_real xoff, flx_real yoff, flx_real crowd, flx_real width, flx_real height, flx_real zoom) {
  if (grabbed()) {
    scenenode *n = grabbed_node();
    if (n && n->m_primitive!=NULL) {
      // todo: check
      ((text_primitive*)(n->m_primitive))->set_text_params(w, h, stride, wrap, xoff, yoff, zoom);
      ((text_primitive*)(n->m_primitive))->set_text(text, crowd, width, height);

    }
  }
}


mat44 *engine::get_transform() {
  if (grabbed()) {
    return &grabbed_node()->m_tx;
  } else {
    return &state_top()->m_tx;
  }
}

mat44 engine::get_global_transform() {
  if (grabbed()) {
    return m_sg->get_global_transform(grabbed_node());
  } else {
    return state_top()->m_tx;
  }
}

void engine::apply_transform() {
  if (grabbed()) {
    scenenode *n = grabbed_node();
    if (n && n->m_primitive!=NULL) {
      n->m_primitive->apply(n->m_tx);
      n->m_tx.init();
    }
  }
}

unsigned int engine::pdata_size() {
  if (grabbed()) {
    scenenode *n = grabbed_node();
    if (n && n->m_primitive!=NULL) {
      return n->m_primitive->pdata_size();
    }
  }
  return 0;
}


void engine::pdata_add(const char *name) {
  if (grabbed()) {
    scenenode *n = grabbed_node();
    if (n && n->m_primitive!=NULL) {
      vec3* data = new vec3[n->m_primitive->pdata_size()];
      // todo: EE code: (vec3*)memalign(128, sizeof(vec3) * m_size

      n->m_primitive->pdata_add(name,data);
    }
  }
}

vec3 *engine::pdata_get(const char *name, int i) {
  if (grabbed()) {
    scenenode *n = grabbed_node();
    if (n && n->m_primitive!=NULL) {
      return n->m_primitive->pdata_get(name,i);
    }
  }
  return NULL;
}

void engine::pdata_set(const char *name, int i, vec3 v) {
  if (grabbed()) {
    scenenode *n = grabbed_node();
    if (n && n->m_primitive!=NULL) {
      n->m_primitive->pdata_set(name,i,v);
    }
  }
}

void engine::recalc_bb() {
  if (grabbed()) {
    scenenode *n = grabbed_node();
    if (n && n->m_primitive!=NULL) {
      n->m_primitive->recalc_bb();
    }
  }
  return;
}

bool engine::bb_point_intersect(const vec3 &p, flx_real threshold) {
  if (grabbed()) {
    scenenode *n = grabbed_node();
    if (n && n->m_primitive!=NULL) {
      vec3 pt=m_sg->get_global_transform(n).inverse().transform(p);
      return n->m_primitive->intersect_bb(pt,threshold);
    }
  }
  return false;
}

bb::list *engine::geo_line_intersect(const vec3 &start, const vec3 &end) {
  if (grabbed()) {
    scenenode *n = grabbed_node();
    if (n && n->m_primitive!=NULL) {
      return n->m_primitive->intersect(start,end);
    }
  }
  return NULL;
}

u32 engine::get_line_intersect(const vec3 &start, const vec3 &end) {
  if (grabbed()) {
    scenenode *n = grabbed_node();
    if (n) {
      return m_sg->intersect_node_walk(n,start,end);
    }
  }
  return -1;
}

u32 engine::load_texture(const char *filename, u32 w, u32 h, u8* data) {
  return m_sg->m_texture_manager.load(filename,w,h,data);
}

u32 engine::get_texture(const char *filename) {
  m_sg->m_texture_manager.find(filename);
}

void engine::render() {
#ifdef _EE
  ps2_renderer::get()->set_camera(m_camera_tx.arr());
#else
  /*
    flx_real buf[4];
    glEnable(GL_LIGHT0);
    buf[0]=0.1f; buf[1]=0.1f; buf[2]=0.1f;  buf[3]=1.f;
    glLightxv(GL_LIGHT0, GL_AMBIENT, (GLfixed *)buf);
    buf[0]=0.5f; buf[1]=0.5f; buf[2]=0.5f;  buf[3]=1.f;
    glLightxv(GL_LIGHT0, GL_DIFFUSE, (GLfixed *)buf);
    buf[0]=1.0f; buf[1]=1.0f; buf[2]=1.0f;  buf[3]=1.1f;
    glLightxv(GL_LIGHT0, GL_SPECULAR, (GLfixed *)buf);
    buf[0]=0.0f; buf[1]=0.0f; buf[2]=0.0f;  buf[3]=1.1f;
    glLightxv(GL_LIGHT0, GL_POSITION, (GLfixed *)buf);

    glMultMatrixx((GLfixed*)&m_camera_tx.m[0][0]);

    if (m_attached_prim!=NULL) {
    glMultMatrixx((GLfixed*)&m_attached_prim->m_tx.inverse().m[0][0]);
    }
  */


  glMultMatrixf((flx_real*)&m_camera_tx.m[0][0]);

  if (m_attached_prim!=NULL) {
    glMultMatrixf((flx_real*)&m_attached_prim->m_tx.inverse().m[0][0]);
  }


  flx_real buf[4];

  glEnable(GL_LIGHT0);
  buf[0]=0.0f; buf[1]=0.0f; buf[2]=0.0f;  buf[3]=1.0f;
  glLightfv(GL_LIGHT0, GL_AMBIENT, (flx_real*)buf);
  buf[0]=0.4f; buf[1]=0.4f; buf[2]=0.4f;  buf[3]=1.0f;
  glLightfv(GL_LIGHT0, GL_DIFFUSE, (flx_real*)buf);
  buf[0]=1.0f; buf[1]=1.0f; buf[2]=1.0f;  buf[3]=1.0f;
  glLightfv(GL_LIGHT0, GL_SPECULAR, (flx_real*)buf);
  buf[0]=0.0f; buf[1]=0.0f; buf[2]=0.0f;  buf[3]=1.0f;
  glLightfv(GL_LIGHT0, GL_POSITION, (flx_real*)buf);

  glEnable(GL_LIGHT1);
  buf[0]=0.0f; buf[1]=0.0f; buf[2]=0.0f;  buf[3]=1.0f;
  glLightfv(GL_LIGHT1, GL_AMBIENT, (flx_real*)buf);
  buf[0]=1.0f; buf[1]=1.0f; buf[2]=1.0f;  buf[3]=1.0f;
  glLightfv(GL_LIGHT1, GL_DIFFUSE, (flx_real*)buf);
  buf[0]=1.0f; buf[1]=0.0f; buf[2]=0.0f;  buf[3]=1.0f;
  glLightfv(GL_LIGHT1, GL_SPECULAR, (flx_real*)buf);
  buf[0]=0.0f; buf[1]=0.5f; buf[2]=0.5f;  buf[3]=0.0f;
  glLightfv(GL_LIGHT1, GL_POSITION, (flx_real*)buf);
  

  //    glMultMatrixf(&m_camera_tx.m[0][0]);
#endif
  m_sg->render();

  // turn these off after primitive rendering
  glDisableClientState(GL_NORMAL_ARRAY);
  glDisableClientState(GL_COLOR_ARRAY);
  glDisableClientState(GL_TEXTURE_COORD_ARRAY);
}
