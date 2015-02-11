// https://github.com/chriscamacho/gles2framework

#ifdef FLX_RPI

void doEvents(int __display_width, int __display_height,
	      void (*KeyDownCallback)(unsigned char,int,int),
	      void (*KeyUpCallback)(unsigned char,int,int));

int *getMouse();
void getKeys();
void setMouseRelative(bool mode);

#endif
