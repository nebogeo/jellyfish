AC_INIT(myconfig, version-0.1)
echo "                             Testing for a C compiler"
AC_PROG_CC
echo "                             Testing for a C++ compiler"
AC_PROG_CXX
AC_LANG(C++)
AC_CHECK_HEADERS(iostream)
AC_CHECK_LIB(m, cos)
AC_CHECK_LIB(glut, main)
AC_CHECK_LIB(gl, main)
AC_CHECK_LIB(png, main)
AC_CHECK_LIB(pthread, main)
AC_CHECK_LIB(dl, main)
AC_CHECK_LIB(lo, main)
AC_CHECK_LIB(jpeg, main)

echo "                             Are we on Raspberry Pi?"
AC_CHECK_HEADERS(/opt/vc/include/bcm_host.h)

AC_OUTPUT(Makefile)
