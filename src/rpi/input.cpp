

// code adapted from https://github.com/chriscamacho/gles2framework

#include <stdbool.h>
#include <stdio.h> // sprintf
#include <stdlib.h> // malloc
#include <math.h>
#include <fcntl.h> // open fcntl
#include <unistd.h> // read close
#include <string.h>
#include <dirent.h>
#include "linux/kd.h" // keyboard stuff...
#include "termios.h"
#include "sys/ioctl.h"
#include "linux/input.h"
#include "keys.h"

static struct termios tty_attr_old;
static int old_keyboard_mode;

int __mouse_fd=-1;
int __mouse[3];
bool __rel_mouse;
int __key_fd=0; // defaults to 0 ie console

void restoreKbd() {
  if (__key_fd==0) {
    tcsetattr(0, TCSAFLUSH, &tty_attr_old);
    // ioctl(0, KDSKBMODE, old_keyboard_mode);
    ioctl(0, KDSKBMODE, K_XLATE);
  }
}

void doEvents(int __display_width, int __display_height, 
	      void (*KeyDownCallback)(unsigned char,int,int),
	      void (*KeyUpCallback)(unsigned char,int,int)) {

  if (__key_fd==0) {
    char buf[1];
    int res;

    res = read(__key_fd, &buf[0], 1);
    while (res >= 0) {
      //printf("keyboard %i\n",buf[0]);
      if (buf[0] & 0x80) {
	printf("key %i released\n",(buf[0]&~0x80));
	KeyUpCallback(buf[0]&~0x80,0,0);
      } else {
	printf("key %i pressed\n",(buf[0]&~0x80));
	KeyDownCallback(buf[0]&~0x80,0,0);
      }
      res = read(__key_fd, &buf[0], 1);
    }

  } else {

    struct input_event ev;
    int res;
    res = read(__key_fd, &ev,sizeof(struct input_event));
    while (res>=0) {
      //printf(" %i %i %i\n",ev.type,ev.code,ev.value);
      // should probably handle MSC and SYN as well - meh
      if (ev.type==EV_KEY) {
	
	if (ev.value==1) {
	  //printf("%d\n",lc_map[ev.code]);
	  KeyDownCallback(lc_map[ev.code&0xff],0,0);
	} else if (ev.value==0) {
	  KeyUpCallback(lc_map[ev.code&0xff],0,0);
	}
      }
      res = read(__key_fd, &ev,sizeof(struct input_event));
    }


  }

  if (__rel_mouse) {
    __mouse[0]=0;
    __mouse[1]=0;
  }

  if(__mouse_fd>0) {
    signed char mbuf[3];
    int mres;
    mres=read(__mouse_fd,&mbuf[0],3);
    while(mres>=0) {
      //printf("%i %i %i\n",mbuf[0]&7,mbuf[1],mbuf[2]);
      __mouse[2]=mbuf[0]&7;
      if (__rel_mouse) {
	__mouse[0]=mbuf[1];
	__mouse[1]=-mbuf[2];
      } else {
	__mouse[0]=__mouse[0]+mbuf[1];
	__mouse[1]=__mouse[1]-mbuf[2];
	if (__mouse[0]<0) __mouse[0]=0;
	if (__mouse[1]<0) __mouse[1]=0;
	if (__mouse[0]>__display_width) __mouse[0]=__display_width;
	if (__mouse[1]>__display_height) __mouse[1]=__display_height;
      }

      mres=read(__mouse_fd,&mbuf[0],3);
    }

  }

}


void setMouseRelative(bool mode) {
  __rel_mouse=mode;
}

int *getMouse()
{
  __rel_mouse=false;

  __mouse_fd = open("/dev/input/mouse0", O_RDONLY);
  if (__mouse_fd < 0) {
    printf("open failed\n");
  } else {
    // make none blocking
    int flags = fcntl(__mouse_fd, F_GETFL);
    flags |= O_NONBLOCK;
    fcntl(__mouse_fd, F_SETFL, flags);
  }

  return &__mouse[0];
}

static int __dsort (const struct dirent **a,const struct dirent **b) {
  return 1; // dummy sort
}

static int __dfilter(const struct dirent *d) {
  if (d->d_type==DT_DIR) return 0;
  int i=0;
  i=strlen(d->d_name)-1;
  //printf ("%i %c %c %c \n",d->d_type,d->d_name[i-2],d->d_name[i-1],d->d_name[i]);
  // allegedly usb keyboard symlink *always* ends kbd
  if (d->d_name[i-2]=='k' & d->d_name[i-1]=='b' & d->d_name[i]=='d' ) return 1;
  return 0;
}


void getKeys()
{
  struct dirent **eps;
  int n;

  n = scandir ("/dev/input/by-path/", &eps, __dfilter, __dsort);

  if(n >= 0 && eps != 0 && eps[0] != 0) {
    // only check 1st usb keyboard....
    char fn[256];
    sprintf(fn,"/dev/input/by-path/%s\0",eps[0]->d_name);
    __key_fd=open(fn, O_RDONLY);
    printf("%i %s\n",__key_fd,fn);
  }

  if (__key_fd==-1) __key_fd=0; // on usb evdev failure default to console

  int flags;
  flags = fcntl(__key_fd, F_GETFL);
  flags |= O_NONBLOCK;
  fcntl(__key_fd, F_SETFL, flags);


  if (__key_fd==0) {
    struct termios tty_attr;




    /* save old keyboard mode */
    if (ioctl(__key_fd, KDGKBMODE, &old_keyboard_mode) < 0) {
      //return 0;
      printf("couldn't get the keyboard, are you running via ssh\n");
      printf("or without setting evdev permissions? \n");
    }

    tcgetattr(__key_fd, &tty_attr_old);

    /* turn off buffering, echo and key processing */
    tty_attr = tty_attr_old;
    tty_attr.c_lflag &= ~(ICANON | ECHO | ISIG);
    tty_attr.c_iflag &= ~(ISTRIP | INLCR | ICRNL | IGNCR | IXON | IXOFF);
    tcsetattr(__key_fd, TCSANOW, &tty_attr);

    ioctl(__key_fd, KDSKBMODE, K_RAW);
  }
}

