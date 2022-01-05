#-------------------------------------------------------------------------------
#   Makefile for rtc
#
#   jan.mooiman@deltares.nl
#   09 Feb 2009
#
#   Copyright (C) 2009, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am

bin_PROGRAMS = ddcouple

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/rd_token

#-------------------------------------------------------------------------------
#Sources
ddcouple_SOURCES = \
	dlwqgrid_mod.f90 \
	dlwqdata_mod.f90 \
	domain_mod.f90 \
	wstmod.f90 \
	filmod.f90 \
	hydmod.f90 \
	read_poi.f90 \
	merge_step.f90 \
	pcunfi.f90 \
	dhnlun.f90 \
	write_atr.f90 \
	jbpcfi.f90 \
	read_hyd_init.f90 \
	write_src.f90 \
	read_hyd_step.f90 \
	write_srf.f90 \
	filestack_add.f90 \
	write_lga.f90 \
	write_hyd_step.f90 \
	write_cco.f90 \
	write_overall_dmo.f90 \
	get_token.f90 \
	read_atr.f90 \
	read_src.f90 \
	write_data.f90 \
	read_srf.f90 \
	merge_data.f90 \
	ddc_version.f90 \
	read_lga.f90 \
	read_cco.f90 \
	ddcouple.f90 \
	write_poi.f90 \
	merge_atr.f90 \
	read_hyd.f90 \
	from_ddb1.f90 \
	from_ddb2.f90 \
	set_hyd.f90 \
	read_ddb.f90 \
	write_hyd.f90


#-------------------------------------------------------------------------------
#Libraries

ddcouple_LDADD   = \
	$(top_srcdir)/tools/ddcouple/packages/ddcouple_version_number/src/libddcouple_version_number.a \
	$(top_srcdir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils/libwaq_utils.la \
    $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la \
    $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/rd_token/libRDToken.la \
	$(FCLIBS) \
	$(X_PRE_LIBS) $(XPLIB) $(X_LIBS) $(X_EXTRA_LIBS)
