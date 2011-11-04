#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Irv.Elshoff@deltares.nl
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   11 jan 2007
#
#   Copyright (C) 2007, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LIBRARIES = libprecision.a

#-------------------------------------------------------------------------------
#Includes

#-------------------------------------------------------------------------------
#Sources
libprecision_a_SOURCES = \
         precision_version.F90 \
         precision_basics.f90 \
         precision.f90 \
         precision_sp.f90


BUILT_SOURCES = ./always \
	./precision_version.F90 

./always:
	rm -rf ./precision_version.F90 

./precision_version.F90:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh $(srcdir)/precision_version.F90 $(srcdir) ../include/version_number.ini $(top_builddir)

