include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libDhydrologyIO.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_srcdir) \
	-I$(top_builddir)/utils_gpl/dhydrology/include \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/include

FMODULES += \
   -I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_gpl/dhydrology/packages/dhydrology_kernel/src

#-------------------------------------------------------------------------------
#Sources
libMorphologyIO_la_SOURCES = \
         dhydrology_reader.f90

#-------------------------------------------------------------------------------
#Libraries

EXTRALIBS=

libDhydrologyIO_la_LIBADD=\
        $(EXTRALIBS)
