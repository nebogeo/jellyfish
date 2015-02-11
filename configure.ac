AC_INIT(jellyfish, version-0.1)
AC_PROG_CC
CFLAGS="$CFLAGS -I/opt/vc/include/"
LDFLAGS="$LDFLAGS -L/opt/vc/lib"
dnl FIXME: EGL of RPi depends on GLESv1 or GLESv2
dnl FIXME: GLESv2 of RPi depends on EGL... WTF!
AC_CHECK_LIB([bcm_host], [bcm_host_init], [

CFLAGS="$CFLAGS -DFLX_RPI -I/opt/vc/include/interface/vcos/pthreads/ -I/opt/vc/include/interface/vmcs_host/linux/"
AC_CHECK_LIB(X11, main)
AC_CHECK_LIB(GLESv1_CM, main)
AC_CHECK_LIB(EGL, eglCopyBuffers)

])

AC_CHECK_LIB(m, cos)
AC_CHECK_LIB(glut, main)
AC_CHECK_LIB(gl, main)
AC_CHECK_LIB(png, main)
AC_CHECK_LIB(pthread, main)
AC_CHECK_LIB(dl, main)
AC_CHECK_LIB(lo, main)
AC_CHECK_LIB(jpeg, main)
AC_CHECK_LIB(asound, main)

AC_PROG_CXX
AC_LANG(C++)
AC_CHECK_HEADERS(iostream)

AC_CONFIG_HEADER(src/config.h)
AC_OUTPUT(Makefile)
