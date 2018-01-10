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

// messy scheme interface

#include <unistd.h>
#include <sys/time.h>
#include <time.h>
#include "scheme.h"
#include "scheme-private.h"
///// starwisp stuff //////////////////////////

#ifdef ANDROID_NDK
#include <android/log.h>
#endif

#include "engine/engine.h"
#include "engine/shader.h"
#include "core/geometry.h"
#include "core/osc.h"
#include "fluxa/graph.h"
#include "fluxa/time.h"
#include "audio.h"

graph *m_audio_graph = NULL;
audio_device *m_audio_device = NULL;

char *starwisp_data = NULL;

#ifdef USE_SQLITE
#include "core/db_container.h"
db_container the_db_container;
#include "core/idmap.h"
idmap the_idmap;
#endif

pointer scheme_interface(scheme *sc, enum scheme_opcodes op) {
  switch (op) {
    ///////////// FLUXUS
  case OP_ALOG:
#ifdef ANDROID_NDK
    __android_log_print(ANDROID_LOG_INFO, "starwisp", string_value(car(sc->args)));
#endif
    s_return(sc,sc->F);
  case OP_SEND:
    if (is_string(car(sc->args))) {
      if (starwisp_data!=NULL) {
#ifdef ANDROID_NDK
	__android_log_print(ANDROID_LOG_INFO, "starwisp", "deleting starwisp data: something is wrong!");
#endif
	free(starwisp_data);
      }
      starwisp_data=strdup(string_value(car(sc->args)));
    }
    s_return(sc,sc->F);
  case OP_OPEN_DB: {
#ifdef USE_SQLITE
    if (is_string(car(sc->args))) {
      the_db_container.add(string_value(car(sc->args)),
			   new db(string_value(car(sc->args))));
      s_return(sc,sc->T);
    }
#endif
    s_return(sc,sc->F);
  }
  case OP_EXEC_DB: {
#ifdef USE_SQLITE
    if (is_string(car(sc->args)) &&
	is_string(cadr(sc->args))) {
      db *d=the_db_container.get(string_value(car(sc->args)));
      if (d!=NULL)
	{
	  s_return(sc,db_exec(sc,d));
	}
    }
#endif
    s_return(sc,sc->F);
  }
  case OP_INSERT_DB: {
#ifdef USE_SQLITE
    if (is_string(car(sc->args)) &&
	is_string(cadr(sc->args))) {
      db *d=the_db_container.get(string_value(car(sc->args)));
      if (d!=NULL)
	{
	  db_exec(sc,d);
	  s_return(sc,mk_integer(sc,d->last_rowid()));
	}
    }
#endif
    s_return(sc,sc->F);
  }
    /*     case OP_INSERT_BLOB_DB: {
	   #ifndef FLX_RPI
	   if (is_string(car(sc->args)) &&
	   is_string(caddr(sc->args)) &&
	   is_string(cadddr(sc->args)) &&
	   is_string(caddddr(sc->args)) &&
	   is_string(cadddddr(sc->args))) {
	   db *d=the_db_container.get(string_value(car(sc->args)));
	   if (d!=NULL)
	   {
	   db_exec(sc,d);
	   s_return(sc,mk_integer(sc,d->last_rowid()));
	   }
	   }
	   #endif
	   s_return(sc,sc->F);
	   } */
  case OP_STATUS_DB: {
#ifdef USE_SQLITE
    if (is_string(car(sc->args))) {
      s_return(sc,mk_string(sc,the_db_container.status()));
    }
#endif
    s_return(sc,sc->F);
  }
  case OP_TIME: {
    timeval t;
    // stop valgrind complaining
    t.tv_sec=0;
    t.tv_usec=0;
    gettimeofday(&t,NULL);
    s_return(sc,cons(sc,mk_integer(sc,t.tv_sec),
		     cons(sc,mk_integer(sc,t.tv_usec),sc->NIL)));
  }
  case OP_NTP_TIME: {
    spiralcore::time t;
    t.set_to_now();
    s_return(sc,cons(sc,mk_integer(sc,t.seconds),
		     cons(sc,mk_integer(sc,t.fraction),sc->NIL)));
  }
  case OP_NTP_TIME_ADD: {
    spiralcore::time t(ivalue(car(car(sc->args))),
		       ivalue(cadr(car(sc->args))));

    t+=rvalue(cadr(sc->args));

    s_return(sc,cons(sc,mk_integer(sc,t.seconds),
		     cons(sc,mk_integer(sc,t.fraction),sc->NIL)));
  }
  case OP_NTP_TIME_DIFF: {
    spiralcore::time t(ivalue(car(car(sc->args))),
		       ivalue(cadr(car(sc->args))));
    spiralcore::time t2(ivalue(car(cadr(sc->args))),
			ivalue(cadr(cadr(sc->args))));
       
    s_return(sc,mk_real(sc,t.get_difference(t2)));
  }
  case OP_NTP_TIME_GTR: {
    spiralcore::time t(ivalue(car(car(sc->args))),
		       ivalue(cadr(car(sc->args))));
    spiralcore::time t2(ivalue(car(cadr(sc->args))),
			ivalue(cadr(cadr(sc->args))));
    if (t>t2) s_return(sc,sc->T);
    else s_return(sc,sc->F);
  }

  case OP_DATETIME: {
    timeval t;
    // stop valgrind complaining
    t.tv_sec=0;
    t.tv_usec=0;
    gettimeofday(&t,NULL);

    struct tm *now = gmtime((time_t *)&t.tv_sec);

    /* note: now->tm_year is the number of years SINCE 1900.  On the year 2000, this
       will be 100 not 0.  Do a man gmtime for more information */

    s_return(sc,cons(sc,mk_integer(sc,now->tm_year + 1900),
		     cons(sc,mk_integer(sc,now->tm_mon + 1),
			  cons(sc,mk_integer(sc,now->tm_mday),
			       cons(sc,mk_integer(sc,now->tm_hour),
				    cons(sc,mk_integer(sc,now->tm_min),
					 cons(sc,mk_integer(sc,now->tm_sec), sc->NIL)))))));

  }

#ifdef USE_SQLITE
  case OP_ID_MAP_ADD: {
    the_idmap.add(
		  string_value(car(sc->args)),
		  ivalue(cadr(sc->args)));
    s_return(sc,sc->F);
  }
  case OP_ID_MAP_GET: {
    s_return(
	     sc,mk_integer(sc,the_idmap.get(
					    string_value(car(sc->args)))));
  }
#endif

    //////////////////// fluxa /////////////////////////////////////////
  case OP_SYNTH_INIT: {
    // name,buf,sr,synths
    m_audio_device = new audio_device(string_value(car(sc->args)),
				      ivalue(cadr(sc->args)),
				      ivalue(caddr(sc->args)),
				      ivalue(cadddr(sc->args)));
    m_audio_graph = new graph(ivalue(caddddr(sc->args)),ivalue(caddr(sc->args)));
    m_audio_device->start_graph(m_audio_graph);
    s_return(sc,sc->F);
  } break;
  case OP_AUDIO_CHECK: {
    m_audio_device->check_audio();
    s_return(sc,sc->F);
  } break;
  case OP_SYNTH_RECORD: {
    m_audio_device->start_recording(string_value(car(sc->args)));
    s_return(sc,sc->F);
  } break;
  case OP_AUDIO_EQ: {
    m_audio_device->m_left_eq.set_low(rvalue(car(sc->args)));
    m_audio_device->m_right_eq.set_low(rvalue(car(sc->args)));
    m_audio_device->m_left_eq.set_mid(rvalue(cadr(sc->args)));
    m_audio_device->m_right_eq.set_mid(rvalue(cadr(sc->args)));
    m_audio_device->m_left_eq.set_high(rvalue(caddr(sc->args)));
    m_audio_device->m_right_eq.set_high(rvalue(caddr(sc->args)));
    s_return(sc,sc->F);
  } break;
  case OP_AUDIO_COMP: {
    m_audio_device->m_left_comp.set_attack(rvalue(car(sc->args)));
    m_audio_device->m_right_comp.set_attack(rvalue(car(sc->args)));
    m_audio_device->m_left_comp.set_release(rvalue(cadr(sc->args)));
    m_audio_device->m_right_comp.set_release(rvalue(cadr(sc->args)));
    m_audio_device->m_left_comp.set_threshold(rvalue(caddr(sc->args)));
    m_audio_device->m_right_comp.set_threshold(rvalue(caddr(sc->args)));
    m_audio_device->m_left_comp.set_slope(rvalue(cadddr(sc->args)));
    m_audio_device->m_right_comp.set_slope(rvalue(cadddr(sc->args)));
    s_return(sc,sc->F);
  } break;
  case OP_SYNTH_CRT: {
    m_audio_graph
      ->create(ivalue(car(sc->args)),
	       (graph::node_type)(ivalue(cadr(sc->args))),
	       rvalue(caddr(sc->args)));
    s_return(sc,sc->F);
  } break;
  case OP_SYNTH_CON: {
    m_audio_graph
      ->connect(ivalue(car(sc->args)),
		ivalue(cadr(sc->args)),
		ivalue(caddr(sc->args)));
    s_return(sc,sc->F);
  } break;
  case OP_SYNTH_PLY: {
    m_audio_graph
      ->play(ivalue(car(sc->args)),
	     ivalue(cadr(sc->args)),
	     ivalue(caddr(sc->args)),
	     rvalue(cadddr(sc->args)));
    s_return(sc,sc->F);
  } break;
  case OP_SLEEP: {
    usleep(ivalue(car(sc->args)));
    s_return(sc,sc->F);
  } break;
  case OP_FMOD: {
    s_return(sc,mk_real(sc,fmod(rvalue(car(sc->args)),rvalue(cadr(sc->args)))));
  } break;

  case OP_OSC_SEND: {
    const char *url=string_value(car(sc->args));
    const char *name=string_value(cadr(sc->args));
    const char *types=string_value(caddr(sc->args));

    pointer data=cadddr(sc->args);

    // figure out size of the data packet
    u32 data_size=0;
    for (u32 i=0; i<strlen(types); ++i) {
      switch(types[i]) {
      case 'f': data_size+=sizeof(float); break;
      case 'i': data_size+=sizeof(int); break;
      case 'l': data_size+=sizeof(long long); break;
      case 's': data_size+=strlen(string_value(list_ref(sc,data,i)))+1; break;
      }
    }

    // build data packet
    char *packet = new char[data_size];
    u32 data_pos=0;
    for (u32 i=0; i<strlen(types); ++i) {
      switch(types[i]) {
      case 'f': {
	float v=rvalue(list_ref(sc,data,i));
	memcpy(packet+data_pos,&v,sizeof(float));
	data_pos+=sizeof(float);
      } break;
      case 'i': { 
	int v=ivalue(list_ref(sc,data,i));
	memcpy(packet+data_pos,&v,sizeof(int));
	data_pos+=sizeof(int); 
      } break;
      case 'l': 
	/*float v=ivalue(list_ref(sc,data,i));
	memcpy(packet+data_pos,&v,sizeof(float));
	data_pos+=sizeof(long long); */
	break;
      case 's': {
	char *str=string_value(list_ref(sc,data,i));
	memcpy(packet+data_pos,str,strlen(str));
	data_pos+=strlen(string_value(list_ref(sc,data,i)));
	packet[data_pos]=0; // null terminator
	data_pos++;
      } break;
      }
    }
    
    network_osc::send(url,name,types,packet,data_size);

    delete[] packet;

    s_return(sc,sc->F);
  } break;

    //////////////////// fluxus /////////////////////////////////////////

  case OP_PUSH:
    engine::get()->push(); s_return(sc,sc->F);
  case OP_POP:
    engine::get()->pop(); s_return(sc,sc->F);
  case OP_GRAB:
    engine::get()->grab(ivalue(car(sc->args))); s_return(sc,sc->F);
  case OP_UNGRAB:
    engine::get()->ungrab(); s_return(sc,sc->F);
  case OP_PARENT:
    engine::get()->parent(ivalue(car(sc->args))); s_return(sc,sc->F);
  case OP_LOCK_CAMERA:
    engine::get()->lock_camera(ivalue(car(sc->args))); s_return(sc,sc->F);
  case OP_IDENTITY:
    engine::get()->identity(); s_return(sc,sc->F);
  case OP_TRANSLATE:
    engine::get()->translate(rvalue(vector_elem(car(sc->args),0)),
			     rvalue(vector_elem(car(sc->args),1)),
			     rvalue(vector_elem(car(sc->args),2)));
    s_return(sc,sc->F);
  case OP_SCALE:
    if (!is_vector(car(sc->args))) // uniform scale with one arg
      {
	engine::get()->scale(rvalue(car(sc->args)),
			     rvalue(car(sc->args)),
			     rvalue(car(sc->args)));
      } else {
      engine::get()->scale(rvalue(vector_elem(car(sc->args),0)),
			   rvalue(vector_elem(car(sc->args),1)),
			   rvalue(vector_elem(car(sc->args),2)));
    }
    s_return(sc,sc->F);
  case OP_ROTATE:
    engine::get()->rotate(rvalue(vector_elem(car(sc->args),0)),
			  rvalue(vector_elem(car(sc->args),1)),
			  rvalue(vector_elem(car(sc->args),2)));
    s_return(sc,sc->F);
  case OP_AIM:
    engine::get()->aim(rvalue(vector_elem(car(sc->args),0)),
		       rvalue(vector_elem(car(sc->args),1)),
		       rvalue(vector_elem(car(sc->args),2)),
		       rvalue(vector_elem(cadr(sc->args),0)),
		       rvalue(vector_elem(cadr(sc->args),1)),
		       rvalue(vector_elem(cadr(sc->args),2)));
    s_return(sc,sc->F);
  case OP_CONCAT:
    {
      mat44 t = mat44(rvalue(vector_elem(car(sc->args),0)),
		      rvalue(vector_elem(car(sc->args),1)),
		      rvalue(vector_elem(car(sc->args),2)),
		      rvalue(vector_elem(car(sc->args),3)),
		      rvalue(vector_elem(car(sc->args),4)),
		      rvalue(vector_elem(car(sc->args),5)),
		      rvalue(vector_elem(car(sc->args),6)),
		      rvalue(vector_elem(car(sc->args),7)),
		      rvalue(vector_elem(car(sc->args),8)),
		      rvalue(vector_elem(car(sc->args),9)),
		      rvalue(vector_elem(car(sc->args),10)),
		      rvalue(vector_elem(car(sc->args),11)),
		      rvalue(vector_elem(car(sc->args),12)),
		      rvalue(vector_elem(car(sc->args),13)),
		      rvalue(vector_elem(car(sc->args),14)),
		      rvalue(vector_elem(car(sc->args),15)));
      t.transpose();
      engine::get()->concat(t);
      s_return(sc,sc->F);
    }
  case OP_COLOUR:
    engine::get()->colour(rvalue(vector_elem(car(sc->args),0)),
			  rvalue(vector_elem(car(sc->args),1)),
			  rvalue(vector_elem(car(sc->args),2)),
			  rvalue(vector_elem(car(sc->args),3)));
    s_return(sc,sc->F);
  case OP_HINT:
    {
      u32 h=ivalue(car(sc->args));
      switch (h)
	{
	case 0: engine::get()->hint(HINT_NONE); break; //???
	case 1: engine::get()->hint(HINT_SOLID); break;
	case 2: engine::get()->hint(HINT_WIRE); break;
	case 3: engine::get()->hint(HINT_NORMAL); break;
	case 4: engine::get()->hint(HINT_POINTS); break;
	case 5: engine::get()->hint(HINT_AALIAS); break;
	case 6: engine::get()->hint(HINT_BOUND); break;
	case 7: engine::get()->hint(HINT_UNLIT); break;
	case 8: engine::get()->hint(HINT_VERTCOLS); break;
	case 9: engine::get()->hint(HINT_ORIGIN); break;
	case 10: engine::get()->hint(HINT_CAST_SHADOW); break;
	case 11: engine::get()->hint(HINT_IGNORE_DEPTH); break;
	case 12: engine::get()->hint(HINT_DEPTH_SORT); break;
	case 13: engine::get()->hint(HINT_LAZY_PARENT); break;
	case 14: engine::get()->hint(HINT_CULL_CCW); break;
	case 15: engine::get()->hint(HINT_WIRE_STIPPLED); break;
	case 16: engine::get()->hint(HINT_SPHERE_MAP); break;
	case 17: engine::get()->hint(HINT_FRUSTUM_CULL); break;
	case 18: engine::get()->hint(HINT_NORMALISE); break;
	case 19: engine::get()->hint(HINT_NOBLEND); break;
	case 20: engine::get()->hint(HINT_NOZWRITE); break;
	}
      s_return(sc,sc->F);
    }
  case OP_DESTROY:
    engine::get()->destroy(rvalue(car(sc->args)));
    s_return(sc,sc->F);
  case OP_LINE_WIDTH:
    engine::get()->line_width(rvalue(car(sc->args)));
    s_return(sc,sc->F);
  case OP_TEXTURE:
    engine::get()->texture(rvalue(car(sc->args)));
    s_return(sc,sc->F);
          
  case OP_SHADER:
    engine::get()->set_shader(string_value(car(sc->args)),
			      string_value(cadr(sc->args)));
    s_return(sc,sc->F);
  case OP_SHADER_SET: {
    shader *shader = engine::get()->get_current_shader();
    shader->apply();
    char *name = string_value(car(sc->args));
    pointer arg=cadr(sc->args);
    if (is_vector(arg)) {
      switch (ivalue(arg)) {
      case 3: {
	vec3 vec(rvalue(vector_elem(arg,0)),
		 rvalue(vector_elem(arg,1)),
		 rvalue(vector_elem(arg,2)));
	shader->set_vector(name,vec);
      } break;
      case 16: {
	mat44 mat(rvalue(vector_elem(arg,0)),
		  rvalue(vector_elem(arg,1)),
		  rvalue(vector_elem(arg,2)),
		  rvalue(vector_elem(arg,3)),
		  rvalue(vector_elem(arg,4)),
		  rvalue(vector_elem(arg,5)),
		  rvalue(vector_elem(arg,6)),
		  rvalue(vector_elem(arg,7)),
		  rvalue(vector_elem(arg,8)),
		  rvalue(vector_elem(arg,9)),
		  rvalue(vector_elem(arg,10)),
		  rvalue(vector_elem(arg,11)),
		  rvalue(vector_elem(arg,12)),
		  rvalue(vector_elem(arg,13)),
		  rvalue(vector_elem(arg,14)),
		  rvalue(vector_elem(arg,15)));
	shader->set_matrix(name,mat);
      } break;
      }
    } else {
      if (is_number(arg)) {
	if (num_is_integer(arg))
	  {
	    shader->set_int(name,ivalue(arg));
	  }
	else
	  {
	    shader->set_float(name,rvalue(arg));
	  }
      }
    }
    shader->unapply();
    s_return(sc,sc->F);
  }
  case OP_LOAD_TEXTURE:
    s_return(sc,mk_integer(sc,engine::get()->get_texture(string_value(car(sc->args)))));
  case OP_DRAW_INSTANCE:
    engine::get()->draw_instance(ivalue(car(sc->args)));
    s_return(sc,sc->F);
  case OP_BUILD_CUBE:
    s_return(sc,mk_integer(sc,engine::get()->build_cube()));
  case OP_LOAD_OBJ:
    s_return(sc,mk_integer(sc,engine::get()->load_obj(string_value(car(sc->args)))));
  case OP_RAW_OBJ:
    s_return(sc,mk_integer(sc,engine::get()->raw_obj(string_value(car(sc->args)))));
  case OP_BUILD_TEXT:
    s_return(sc,mk_integer(sc,engine::get()->build_text(
							string_value(car(sc->args)))));
  case OP_BUILD_JELLYFISH:
    s_return(sc,mk_integer(sc,engine::get()->build_jellyfish(ivalue(car(sc->args)))));
  case OP_BUILD_INSTANCE:
    s_return(sc,mk_integer(sc,engine::get()->build_instance(ivalue(car(sc->args)))));
  case OP_BUILD_POLYGONS:
    s_return(sc,mk_integer(sc,engine::get()->build_polygons(
							    ivalue(car(sc->args)),
							    ivalue(cadr(sc->args))
							    )));
  case OP_GET_TRANSFORM:
    {
      flx_real *m=&(engine::get()->get_transform()->m[0][0]);
      pointer v=mk_vector(sc,16);
      int i=0;
      for (i=0; i<16; i++)
	{
	  set_vector_elem(v,i,mk_real(sc,m[i]));
	}
      s_return(sc,v);
    }
  case OP_GET_GLOBAL_TRANSFORM:
    {
      mat44 mat=engine::get()->get_global_transform();
      flx_real *m=&(mat.m[0][0]);
      pointer v=mk_vector(sc,16);
      int i=0;
      for (i=0; i<16; i++)
	{
	  set_vector_elem(v,i,mk_real(sc,m[i]));
	}
      s_return(sc,v);
    }
  case OP_GET_CAMERA_TRANSFORM:
    {
      flx_real *m=&(engine::get()->get_camera_transform()->m[0][0]);
      pointer v=mk_vector(sc,16);
      int i=0;
      for (i=0; i<16; i++)
	{
	  set_vector_elem(v,i,mk_real(sc,m[i]));
	}
      s_return(sc,v);
    }
  case OP_GET_SCREEN_SIZE:
    {
      unsigned int *s=engine::get()->get_screensize();
      pointer v=mk_vector(sc,2);
      set_vector_elem(v,0,mk_real(sc,s[0]));
      set_vector_elem(v,1,mk_real(sc,s[1]));
      s_return(sc,v);
    }
  case OP_APPLY_TRANSFORM:
    engine::get()->apply_transform(); s_return(sc,sc->F);
  case OP_CLEAR:
    engine::get()->clear(); s_return(sc,sc->F);
  case OP_CLEAR_COLOUR:
    engine::get()->clear_colour(rvalue(vector_elem(car(sc->args),0)),
				rvalue(vector_elem(car(sc->args),1)),
				rvalue(vector_elem(car(sc->args),2)),
				rvalue(vector_elem(car(sc->args),3)));
    s_return(sc,sc->F);
  case OP_PDATA_SIZE:
    s_return(sc,mk_integer(sc,engine::get()->pdata_size()));
  case OP_PDATA_ADD:
    engine::get()->pdata_add(string_value(car(sc->args)));
    s_return(sc,sc->F);
  case OP_PDATA_REF:
    {
      vec3* vec=engine::get()->pdata_get(string_value(car(sc->args)),
					 ivalue(cadr(sc->args)));
      pointer v=mk_vector(sc,3);
      if (vec)
	{
	  set_vector_elem(v,0,mk_real(sc,vec->x));
	  set_vector_elem(v,1,mk_real(sc,vec->y));
	  set_vector_elem(v,2,mk_real(sc,vec->z));
	}
      s_return(sc,v);
    }
  case OP_PDATA_SET:
    {
      vec3 vec(rvalue(vector_elem(caddr(sc->args),0)),
	       rvalue(vector_elem(caddr(sc->args),1)),
	       rvalue(vector_elem(caddr(sc->args),2)));
      engine::get()->pdata_set(string_value(car(sc->args)),
			       ivalue(cadr(sc->args)),
			       vec);
      s_return(sc,sc->F);
    }
  case OP_SET_TEXT:
    {
      engine::get()->text_set(string_value(car(sc->args)));
      s_return(sc,sc->F);
    }
  case OP_TEXT_PARAMS:
    {
      engine::get()->text_params(string_value(list_ref(sc,sc->args,0)),
				 rvalue(list_ref(sc,sc->args,1)),
				 rvalue(list_ref(sc,sc->args,2)),
				 ivalue(list_ref(sc,sc->args,3)),
				 ivalue(list_ref(sc,sc->args,4)),
				 rvalue(list_ref(sc,sc->args,5)),
				 rvalue(list_ref(sc,sc->args,6)),
				 rvalue(list_ref(sc,sc->args,7)),
				 rvalue(list_ref(sc,sc->args,8)),
				 rvalue(list_ref(sc,sc->args,9)),
				 rvalue(list_ref(sc,sc->args,10)));
      s_return(sc,sc->F);
    }
  case OP_RECALC_BB:
    {
      engine::get()->recalc_bb();
      s_return(sc,sc->F);
    }
  case OP_BB_POINT_INTERSECT:
    {
      vec3 pvec(rvalue(vector_elem(car(sc->args),0)),
		rvalue(vector_elem(car(sc->args),1)),
		rvalue(vector_elem(car(sc->args),2)));
      s_return(sc,mk_integer(sc,engine::get()->bb_point_intersect(pvec,rvalue(cadr(sc->args)))));
    }
  case OP_GEO_LINE_INTERSECT:
    {
      vec3 svec(rvalue(vector_elem(car(sc->args),0)),
		rvalue(vector_elem(car(sc->args),1)),
		rvalue(vector_elem(car(sc->args),2)));
      vec3 evec(rvalue(vector_elem(cadr(sc->args),0)),
		rvalue(vector_elem(cadr(sc->args),1)),
		rvalue(vector_elem(cadr(sc->args),2)));
      bb::list *points=engine::get()->geo_line_intersect(svec,evec);
      if (points!=NULL)
	{
	  pointer list=sc->NIL;
	  intersect_point *p=static_cast<intersect_point*>(points->m_head);
	  while (p!=NULL)
	    {
	      list=cons(sc,mk_real(sc,p->m_t),list);
	      pointer blend=sc->NIL;
	      intersect_point::blend *b=
		static_cast<intersect_point::blend*>
		(p->m_blends.m_head);
	      while (b!=NULL)
		{
		  pointer v=mk_vector(sc,3);
		  set_vector_elem(v,0,mk_real(sc,b->m_blend.x));
		  set_vector_elem(v,1,mk_real(sc,b->m_blend.y));
		  set_vector_elem(v,2,mk_real(sc,b->m_blend.z));

		  pointer l=sc->NIL;
		  l=cons(sc,mk_string(sc,b->m_name),v);

		  blend=cons(sc,l,blend);
		  b=static_cast<intersect_point::blend*>(b->m_next);
		}
	      list=cons(sc,blend,list);
	      p=static_cast<intersect_point*>(p->m_next);
	    }
	  s_return(sc,list);
	}
      s_return(sc,sc->F);
    }
  case OP_GET_LINE_INTERSECT:
    {
      vec3 svec(rvalue(vector_elem(car(sc->args),0)),
		rvalue(vector_elem(car(sc->args),1)),
		rvalue(vector_elem(car(sc->args),2)));
      vec3 evec(rvalue(vector_elem(cadr(sc->args),0)),
		rvalue(vector_elem(cadr(sc->args),1)),
		rvalue(vector_elem(cadr(sc->args),2)));
      s_return(sc,mk_integer(sc,engine::get()->get_line_intersect(svec,evec)));
    }
  case OP_MINVERSE:
    {
      mat44 inm;
      int i=0;
      for (i=0; i<16; i++)
	{
	  inm.arr()[i]=rvalue(vector_elem(car(sc->args),i));
	}
      inm=inm.inverse();
      pointer v=mk_vector(sc,16);
      for (i=0; i<16; i++)
	{
	  set_vector_elem(v,i,mk_real(sc,inm.arr()[i]));
	}
      s_return(sc,v);
    }
  case OP_BITWISE_IOR:
    {
      s_return(sc,mk_integer(sc,
			     ivalue(car(sc->args))|
			     ivalue(cadr(sc->args))|
			     ivalue(caddr(sc->args))
			     ));
    }
  case OP_BLEND_MODE:
    {
      u32 src = GL_SRC_ALPHA;
      u32 dst = GL_ONE_MINUS_SRC_ALPHA;

      u32 s = ivalue(car(sc->args));
      u32 d = ivalue(cadr(sc->args));
       
      if (s==0) src=GL_ZERO;
      else if (s==1) src=GL_ONE;
      else if (s==2) src=GL_DST_COLOR;
      else if (s==3) src=GL_ONE_MINUS_DST_COLOR;
      else if (s==4) src=GL_SRC_ALPHA;
      else if (s==5) src=GL_ONE_MINUS_SRC_ALPHA;
      else if (s==6) src=GL_DST_ALPHA;
      else if (s==7) src=GL_ONE_MINUS_DST_ALPHA;
      else if (s==8) src=GL_SRC_ALPHA_SATURATE;
        
      if (d==0) dst=GL_ZERO;
      else if (d==1) dst=GL_ONE;
      else if (d==9) dst=GL_SRC_COLOR;
      else if (d==10) dst=GL_ONE_MINUS_SRC_COLOR;
      else if (d==4) dst=GL_SRC_ALPHA;
      else if (d==5) dst=GL_ONE_MINUS_SRC_ALPHA;
      else if (d==6) dst=GL_DST_ALPHA;
      else if (d==7) dst=GL_ONE_MINUS_DST_ALPHA;

      engine::get()->blend_mode(src,dst);
      s_return(sc,sc->F);
    }
  default:
    snprintf(sc->strbuff,STRBUFFSIZE,"%d: illegal operator", sc->op);
    Error_0(sc,sc->strbuff);

  }
  ////////////////////
}
