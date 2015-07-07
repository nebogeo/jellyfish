#include <png.h>
#include <string>
#include "engine/importgl.h"

#ifdef HAVE_LIBJPEG
extern "C"
{
#include <jpeglib.h>
}
#endif

GLubyte *get_screen_buffer(int x, int y, unsigned int width, unsigned int height, int super);

unsigned char* load_png(const std::string filename,long &width, long &height);

#ifdef HAVE_LIBJPEG
int write_jpg(GLubyte *image, const char *filename, const char *description, int x, int y, int width, int height, int quality, int super);
#endif
