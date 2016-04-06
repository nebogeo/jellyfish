AC_INIT(jellyfish, version-0.1)
AC_PROG_CC
CFLAGS="$CFLAGS -I/opt/vc/include/"
LDFLAGS="$LDFLAGS -L/opt/vc/lib"
dnl FIXME: EGL of RPi depends on GLESv1 or GLESv2
dnl FIXME: GLESv2 of RPi depends on EGL... WTF!
AC_CHECK_LIB([bcm_host], [bcm_host_init], [

CFLAGS="$CFLAGS -DFLX_RPI -I/opt/vc/include/interface/vcos/pthreads/ -I/opt/vc/include/interface/vmcs_host/linux/"

AC_CHECK_LIB(bcm_host, main, [], [AC_MSG_ERROR([could not find bcm_host])])
AC_CHECK_LIB(X11, main)
AC_CHECK_LIB(GLESv1_CM, main, [], [AC_MSG_ERROR([could not find GLES])])
AC_CHECK_LIB(EGL, eglCopyBuffers, [], [AC_MSG_ERROR([could not find EGL])])

])

AC_CHECK_LIB(m, cos, [], [AC_MSG_ERROR([could not find libm])])
AC_CHECK_LIB(GLEW, main) dnl no error on Pi
AC_CHECK_LIB(glut, main) dnl no error on Pi
AC_CHECK_LIB(GL, main) dnl no error on Pi
AC_CHECK_LIB(png, main, [], [AC_MSG_ERROR([libpng not found])])
AC_CHECK_LIB(pthread, main, [], [AC_MSG_ERROR([libpthread not found])])
AC_CHECK_LIB(dl, main, [], [AC_MSG_ERROR([libdl was not found])])
AC_CHECK_LIB(lo, main, [], [AC_MSG_ERROR([liblo was not found])])
AC_CHECK_LIB(sndfile, main, [], [AC_MSG_ERROR([sndfile was not found])])
AC_CHECK_LIB(jpeg, main)
AC_CHECK_LIB(portaudio, main, [], [AC_MSG_ERROR([libportaudio not found])])
AC_CHECK_LIB(freetype, main, [], [AC_MSG_ERROR([freetype not found])])
AC_CHECK_LIB(GLU, main, [], [AC_MSG_ERROR([glu not found])])

AC_PROG_CXX
AC_LANG(C++)
AC_CHECK_HEADERS(iostream)

AC_CONFIG_HEADER(src/ac_config.h)
AC_OUTPUT(Makefile)
