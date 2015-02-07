#include <lo/lo.h>

/////////////// osc stuff ////////////////////////////////////////////////

void osc_error_handler(int num, const char *msg, const char *path);
int osc_default_handler(const char *path, const char *types, lo_arg **argv, int argc, void *data, void *user_data);
int osc_eval_handler(const char *path, const char *types, lo_arg **argv,
                     int argc, void *data, void *user_data);
void setup_osc_repl();
