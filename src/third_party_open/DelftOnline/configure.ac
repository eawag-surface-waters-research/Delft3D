#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.68])
AC_INIT([DelftOnline], [0.9], [irv.elshoff@deltares.nl])
AC_CONFIG_SRCDIR([dolsrc/extern/JavaLaunch/include/JavaLaunch.h])
AC_CONFIG_AUX_DIR([config])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([m4])
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE
# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_SED
AC_PROG_INSTALL
AC_PROG_MAKE_SET
AC_PROG_LIBTOOL

#These are required for java (delftonline)
AC_PROG_JAVA
#this is needed when sun java is unavailable
#AM_PROG_GCJ
#AC_PROG_JAVA_CC
AC_PROG_JAVAC
# this should- find the jni.h file (usually in an obscure location), has to be called after JAVAC
AX_PROG_JAVAH
#AC_JNI_INCLUDE_DIR 
AC_MSG_CHECKING([For additional include dirs])
#for JNI_INCLUDE_DIR in $JNI_INCLUDE_DIRS
#do
#	CPPFLAGS="$CPPFLAGS -I$JNI_INCLUDE_DIR"
#	AC_MSG_RESULT([$CPPFLAGS])
#done

# Checks for libraries.
# FIXME: Replace `main' with a function in `-lcrypto':
AC_CHECK_LIB([crypto], [main])

# Checks for header files.
AC_CHECK_HEADERS([fcntl.h stdlib.h string.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_TYPE_PID_T
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_FORK
AC_CHECK_FUNCS([memset])

AC_CONFIG_FILES([Makefile
                 jlsrc/Makefile
		 jlsrc/include/Makefile
                 jlsrc/libsrc/Makefile
                 dolsrc/Makefile
		 dolsrc/include/Makefile
                 dolsrc/libsrc/Makefile
                 ])
                 dnl dolsrc/libsrc/makefile
                 dnl dolsrc/makefile
                 dnl dolsrc/progsrc/doltest/makefile
                 dnl dolsrc/progsrc/printcontents/makefile
                 dnl jlsrc/libsrc/makefile
                 dnl jlsrc/makefile
                 dnl jlsrc/progsrc/makefile
AC_OUTPUT
