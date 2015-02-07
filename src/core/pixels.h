#include <png.h>
#include "engine/importgl.h"

#ifdef USE_JPGLIB
extern "C"
{
#include <jpeglib.h>
}
#endif

GLubyte *GetScreenBuffer(int x, int y, unsigned int width, unsigned int height, int super);
unsigned char* LoadPNG(const string filename,long &width, long &height);
#ifdef USE_JPGLIB
int WriteJPG(GLubyte *image, const char *filename, const char *description, int x, int y, int width, int height, int quality, int super);
#endif
