#-------------------------------------------------------------------------------
#   Makefile for Deltares_hydro
#
#   Irv.Elshoff@deltares.nl
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   31 aug 10
#
#   Copyright (C) 2007-2010, Deltares
#-------------------------------------------------------------------------------


include $(top_srcdir)/common.am

bin_PROGRAMS = \
    deltares_hydro.exe

FINCLUDES += \
	-I$(top_srcdir)/utils_lgpl/esmfsm/include

FMODULES += \
	-I$(top_srcdir)/utils_lgpl/inifiles/packages/inifiles/src

deltares_hydro_exe_SOURCES = \
    f902c.c \
    deltares_hydro_version.F90 \
    deltares_hydro.F90


FCLIBS += \
    -L/opt/mpich2/lib -lmpich



deltares_hydro_exe_LDADD = \
    $(top_srcdir)/utils_lgpl/inifiles/packages/inifiles/src/libinifiles.a \
    $(EXTERNALSOURCES) \
    $(FCLIBS)


BUILT_SOURCES = ./always \
	./deltares_hydro_version.F90 

./always:
	rm -rf ./deltares_hydro_version.F90 

./deltares_hydro_version.F90:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh $(srcdir)/deltares_hydro_version.F90 $(srcdir) ../include/version_number.ini $(top_srcdir)

