#-------------------------------------------------------------------------------
#   Makefile gridgeom
#
#   07 jun 2017
#
#   Copyright (C)  Stichting Deltares, 2011-2017.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libGridgeom.la

# For source distributions: the tar.gz will contain the correct version numbers in the generated *version.F90
# So, just before packaging the .tar.gz dist, disable version number REgeneration parts of this Makefile.am.
dist-hook:
	sed -i -e 's/^REGENERATE_VN[ \t]*=[ \t]*yes/REGENERATE_VN=no/' $(distdir)/Makefile.am
	sed -i -e 's/^REGENERATE_VN[ \t]*=[ \t]*yes/REGENERATE_VN=no/' $(distdir)/Makefile.in

# Version number generation (only when in SVN source dir, not for tar.gz dist)

REGENERATE_VN=yes

# (the space before ifeq is intentional to fool Automake, and leave this conditional to Make)
 ifeq ($(REGENERATE_VN),yes)
# We're in a working copy build dir, so always update the *version.F90
gridgeom_version.F90: gridgeom_version.F90.svn .FORCE
	$(top_builddir)/scripts_lgpl/linux/update_version.sh \
	$(builddir)/$@ $(srcdir) $(srcdir)/../include/version_number.ini $(abs_top_builddir)
 else
# Do not force regeneration of *version.F90, because it is already shipped in a source distribution archive.
gridgeom_version.F90:
 endif

.FORCE: 

.PHONY: .FORCE

#-------------------------------------------------------------------------------
#Includes
AM_FCFLAGS += $(GRIDGEOM_CFLAGS) $(GRIDGEOM_FORTRAN_CFLAGS)

FMODULES += \
    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
    -I$(GRIDGEOMROOT)/include

#-------------------------------------------------------------------------------
#Sources
libGridgeom_la_SOURCES = \
         generalmodules.f90 \
         odugrid.F90 \
         gridgeom.f90 \
         gridgeom_api.F90 \
         gridoperations.F90 \
         network_data.f90 \
         polygon.f90
         

#-------------------------------------------------------------------------------
#Libraries

