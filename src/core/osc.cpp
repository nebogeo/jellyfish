#include "core/osc.h"
#include <pthread.h>
#include <stdio.h>
#include <app.h>

extern pthread_mutex_t* render_mutex;

void osc_error_handler(int num, const char *msg, const char *path)
{
    printf("liblo server error %d in path %s\n",num,path);
}

int osc_default_handler(const char *path, const char *types, lo_arg **argv, int argc, void *data, void *user_data)
{
    //printf("osc server no handler for: %s %s\n",path,types);
	return 1;
}

int osc_eval_handler(const char *path, const char *types, lo_arg **argv,
		    int argc, void *data, void *user_data)
{
    if (types[0]=='s') {
        printf("%s\n",argv[0]);
        pthread_mutex_lock(render_mutex);
        appEval((char*)argv[0]);
        pthread_mutex_unlock(render_mutex);
    }
	return 1;
}

void setup_osc_repl() {
    printf("starting osc, listening to port 8000\n");
	lo_server_thread server = lo_server_thread_new("8000", osc_error_handler);
    lo_server_thread_add_method(server, NULL, NULL, osc_default_handler, NULL);
    lo_server_thread_add_method(server, "/eval", "s", osc_eval_handler, NULL);
	lo_server_thread_start(server);
}
