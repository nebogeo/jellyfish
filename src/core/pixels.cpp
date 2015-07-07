#include "core/pixels.h"

#include <iostream>
using namespace std;

GLubyte *get_screen_buffer(int x, int y, unsigned int width, unsigned int height, int super)
{
	// get the raw image
	GLubyte *image = (GLubyte *) malloc(width * height * sizeof(GLubyte) * 3);
	// OpenGL's default 4 byte pack alignment would leave extra bytes at the
	// end of each image row so that each full row contained a number of bytes
	// divisible by 4.  Ie, an RGB row with 3 pixels and 8-bit componets would
	// be laid out like "RGBRGBRGBxxx" where the last three "xxx" bytes exist
	// just to pad the row out to 12 bytes (12 is divisible by 4). To make sure
	// the rows are packed as tight as possible (no row padding), set the pack
	// alignment to 1.
	glPixelStorei(GL_PACK_ALIGNMENT, 1);
	glReadPixels(x, y, width, height, GL_RGB, GL_UNSIGNED_BYTE, image);

	if (super==1) return image;

	// supersample the image
	int newwidth=width/super;
	int newheight=height/super;

	GLubyte *image2 = (GLubyte *) malloc(newwidth * newheight * sizeof(GLubyte) * 3);

	for (int yy=0; yy<newheight; yy++)
	{
		for (int xx=0; xx<newwidth; xx++)
		{
			int sx=xx*super;
			int sy=yy*super;
			int i=(yy*newwidth+xx)*3;

			int a=(sy*width+sx)*3;
			int b=(sy*width+(sx+1))*3;
			int c=((sy+1)*width+(sx+1))*3;
			int d=((sy+1)*width+sx)*3;

			image2[i]=(image[a]+image[b]+image[c]+image[d])/4;
			image2[i+1]=(image[a+1]+image[b+1]+image[c+1]+image[d+1])/4;
			image2[i+2]=(image[a+2]+image[b+2]+image[c+2]+image[d+2])/4;
		}
	}

	width=newwidth;
	height=newheight;

	free(image);
	return image2;
}


unsigned char* load_png(const string filename,long &width, long &height)
{
	unsigned char *data = NULL;
	FILE *fp=fopen(filename.c_str(),"rb");
	if (!fp || filename=="")
	{
		cerr<<"Couldn't open image ["<<filename<<"]"<<endl;
	}
	else
	{
		png_structp png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
		png_infop info_ptr = png_create_info_struct(png_ptr);

		if (setjmp(png_jmpbuf(png_ptr)))
		{
			png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
			cerr<<"Error reading image ["<<filename<<"]"<<endl;
			fclose(fp);
			return NULL;
		}

		png_init_io(png_ptr, fp);
		png_read_info(png_ptr, info_ptr);

		width = png_get_image_width(png_ptr, info_ptr);
		height = png_get_image_height(png_ptr, info_ptr);
		int bit_depth = png_get_bit_depth(png_ptr, info_ptr);
		int colour_type = png_get_color_type(png_ptr, info_ptr);
		png_bytep *row_pointers=new png_bytep[height];
		unsigned int rb = png_get_rowbytes(png_ptr, info_ptr);

		for (unsigned long row=0; row<height; row++)
		{
			row_pointers[row] = new png_byte[rb];
		}

		// read the data into the row pointers
		png_read_image(png_ptr, row_pointers);
		fclose(fp);

		// make a new contiguous array to store the pixels
		data=new unsigned char[rb*height];
		int p=0;
		for (int row = 0; row<height; row++)
		{
			for (unsigned int i=0; i<rb; i++)
			{
				data[p]=(unsigned char)(row_pointers[row])[i];
				p++;
			}
		}

		// clear up the row_pointers
		for (unsigned long row=0; row<height; row++)
		{
			delete[] row_pointers[row];
		}
		delete[] row_pointers;

		png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp)NULL);
	}
    return data;
}

#ifdef HAVE_LIBJPEG

int write_jpg(GLubyte *image, const char *filename, const char *description, int x, int y, int width, int height, int quality, int super)
{
	struct jpeg_compress_struct cinfo;
	struct jpeg_error_mgr jerr;

 	FILE * outfile;
	JSAMPROW row_pointer[1];
	int row_stride;

	cinfo.err = jpeg_std_error(&jerr);
	jpeg_create_compress(&cinfo);

	if ((outfile = fopen(filename, "wb")) == NULL)
	{
    	return 1;
  	}

	jpeg_stdio_dest(&cinfo, outfile);

 	cinfo.image_width = width;
  	cinfo.image_height = height;
  	cinfo.input_components = 3;
  	cinfo.in_color_space = JCS_RGB;

 	jpeg_set_defaults(&cinfo);
	jpeg_set_quality(&cinfo, quality, TRUE);
	jpeg_start_compress(&cinfo, TRUE);

	row_stride = width * 3;

	while (cinfo.next_scanline < cinfo.image_height)
	{
    	row_pointer[0] = & image[(cinfo.image_height-1-cinfo.next_scanline) * row_stride];
    	(void) jpeg_write_scanlines(&cinfo, row_pointer, 1);
  	}

	jpeg_finish_compress(&cinfo);
 	fclose(outfile);

	jpeg_destroy_compress(&cinfo);
	free(image);

	return 0;
}

#endif
