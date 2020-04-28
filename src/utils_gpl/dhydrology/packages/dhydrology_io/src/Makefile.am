include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libDhydrologyIO.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_srcdir) \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/include

FMODULES += \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_gpl/dhydrology/packages/dhydrology_kernel/src

#-------------------------------------------------------------------------------
#Sources
libDhydrologyIO_la_SOURCES = \
         dhydrology_reader.f90

#-------------------------------------------------------------------------------
#Libraries

EXTRALIBS=
# Do not include ECModule here: leave that to toplevel exe/dll(so) projects,
# to prevent multiple definitions of the same routines.
#\
#	$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src/libECModule.la

libDhydrologyIO_la_LIBADD=\
        $(EXTRALIBS)
