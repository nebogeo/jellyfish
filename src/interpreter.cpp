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

#include <iostream>
#include "interpreter.h"
#include "scheme/scheme.h"

using namespace std;

scheme *interpreter::m_sc = NULL;
FILE *interpreter::m_log_file = NULL;
pthread_mutex_t *interpreter::m_render_mutex=NULL;

interpreter::~interpreter() {
    if (m_log_file!=NULL) fclose(m_log_file);
}

string interpreter::load_file(const string &filename) {
    FILE *file=fopen(filename.c_str(),"r");
	if (file) {
		fseek(file,0,SEEK_END);
		long size=ftell(file);
		fseek(file,0,SEEK_SET);

		char *buffer = new char[size+1];
        long s = (long)fread(buffer,1,size,file);
        buffer[s]='\0';
        string r = buffer;
		delete[] buffer;
		fclose(file);
        return r;
    }
    cerr<<"couldn't open "<<filename<<endl;
    return "";
}

void interpreter::initialise() {
    m_sc=scheme_init_new();

    // where do we want to log to?
#ifdef FLX_LINUX
    m_log_file=stdout;
    scheme_set_input_port_file(m_sc, stdin);
#else
#ifdef FLX_RPI
    m_log_file=stdout;
    scheme_set_input_port_file(m_sc, stdin);
#else // android
    m_log_file=fopen("/sdcard/symbai/symbai-log2.txt","w");
#endif
#endif

    if (m_log_file!=NULL) {
        scheme_set_output_port_file(m_sc, m_log_file);
        fflush(m_log_file);
    }
}

void interpreter::eval(const string &code) {
    scheme_load_string(m_sc,code.c_str());
}

void interpreter::eval_file(const string &filename) {
    scheme_load_string(m_sc,load_file(filename).c_str());
}

void interpreter::start_repl(pthread_mutex_t *render_mutex) {
    m_render_mutex = render_mutex;
    pthread_t *repl_thread = new pthread_t;
    pthread_create(repl_thread,NULL,(void*(*)(void*))repl_loop,NULL);
}

void interpreter::repl_loop() {
    char cmd_str[80];
    do {
        printf("jellyfish> ");
        fgets(cmd_str, 80, stdin);
        pthread_mutex_lock(m_render_mutex);
        eval(cmd_str);
        pthread_mutex_unlock(m_render_mutex);
    } while(1);
}
