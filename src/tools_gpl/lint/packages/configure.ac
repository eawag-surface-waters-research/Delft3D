#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT([lint], [0.00.00], delft3d.support@deltares.nl)
dnl Store generated files in config dir
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_SRCDIR([lint_c/main.cpp])
AM_INIT_AUTOMAKE
AC_CONFIG_HEADER([config.h])


dnl Initialiseer automake
AM_INIT_AUTOMAKE

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_CPP


## ----------------------- ##
## Libtool initialisation. ##
## ----------------------- ##
dnl Libtool is disabled (static linking issues)
#LT_INIT -> new in 2.0
#AC_PROG_LIBTOOL
#AC_SUBST([LIBTOOL_DEPS])


## This is copied from the libtool examples
## ------------------------ ##
## Fortran compiler checks. ##
## ------------------------ ##
AC_LANG_PUSH([Fortran])
dnl http://www.astro.gla.ac.uk/users/norman/star/ssn78/ssn78.htx/N-x1a1b12.html
AC_PROG_F77([ifort ifc g95 g77 f77 gfortran])
AC_PROG_FC([ifort ifc g95gfortran])
dnl Check the flags needed to link fc programs with ld (i.e. cc)
AC_FC_LIBRARY_LDFLAGS
dnl Check for underscoring of external names
AC_FC_WRAPPERS
dnl We need to use .f90 and not .f to enable Automake FC support
dnl Some Intel ifc/ifort do not understand .f95.  :-/
AC_FC_SRCEXT([f90])
dnl Yes, we want free form Fortran!
AC_FC_FREEFORM
AC_LANG_POP


# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

#which makefiles are we gonna make?
AC_CONFIG_FILES([Makefile
                 lint_c/Makefile]
                 lint_f/Makefile])
AC_OUTPUT
