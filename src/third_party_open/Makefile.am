#-------------------------------------------------------------------------------
#   Makefile for ds/third_party
#
#   Adri.Mourits@deltares.nl
#   25 sep 2008
#
#   Copyright (C) 2008, Deltares
#
#   To enable compilation in third_party:
#   Add third_party to the SUBDIRS line in file ds/Makefile.am
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am

# Descending into FortranGIS is optional, since its prerequisites
# --with-shapelib and --with-proj are optional.
if USEFORTRANGIS
  MAYBE_FORTRANGIS = fortrangis
endif

SUBDIRS = \
    esmf \
    triangle \
    version_number \
    libsigwatch \
    FLAP \
    polypack \
    kdtree2 \
    md5 \
    $(MAYBE_FORTRANGIS)
nobase_dist_pkgdata_DATA = \
    esmf/lnx64/bin/libcilkrts.so.5 \
    esmf/lnx64/bin/libdl.so.2 \
    esmf/lnx64/bin/libesmf.so \
    esmf/lnx64/bin/libgcc_s.so.1 \
    esmf/lnx64/bin/libhdf5.so.8 \
    esmf/lnx64/bin/libhdf5_hl.so.8 \
    esmf/lnx64/bin/libifcore.so.5 \
    esmf/lnx64/bin/libifport.so.5 \
    esmf/lnx64/bin/libimf.so \
    esmf/lnx64/bin/libintlc.so.5 \
    esmf/lnx64/bin/libiomp5.so \
    esmf/lnx64/bin/libirc.so \
    esmf/lnx64/bin/libirng.so \
    esmf/lnx64/bin/libm.so.6 \
    esmf/lnx64/bin/libnetcdf.so.7 \
    esmf/lnx64/bin/libnetcdff.so.6 \
    esmf/lnx64/bin/librt.so.1 \
    esmf/lnx64/bin/libsvml.so \
    esmf/lnx64/bin/libz.so.1 \
    esmf/lnx64/bin_COS7/libcilkrts.so.5 \
    esmf/lnx64/bin_COS7/libesmf.so \
    esmf/lnx64/bin_COS7/libhdf5.so.200 \
    esmf/lnx64/bin_COS7/libhdf5_hl.so.200 \
    esmf/lnx64/bin_COS7/libifcoremt.so.5 \
    esmf/lnx64/bin_COS7/libifport.so.5 \
    esmf/lnx64/bin_COS7/libimf.so \
    esmf/lnx64/bin_COS7/libintlc.so.5 \
    esmf/lnx64/bin_COS7/libiomp5.so \
    esmf/lnx64/bin_COS7/libirc.so \
    esmf/lnx64/bin_COS7/libirng.so \
    esmf/lnx64/bin_COS7/libnetcdf.so.7 \
    esmf/lnx64/bin_COS7/libnetcdf.so.18 \
    esmf/lnx64/bin_COS7/libnetcdff.so.6 \
    esmf/lnx64/bin_COS7/libsvml.so

