# clean up
make distclean
# build src/config.h from configure.ac
autoheader
# build configure
autoconf configure.ac > configure
# make configure executable
chmod +x configure
