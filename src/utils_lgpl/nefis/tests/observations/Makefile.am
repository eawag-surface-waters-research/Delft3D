#-------------------------------------------------------------------------------
#
#   Jan.Mooiman@deltares.nl
#   26 oct 2007
#
#   Copyright (C) 2007, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = observ

observ_SOURCES = \
	observ.f
observ_LDADD = \
	../../packages/nefis/src/libNefis.la \
	$(FCLIBS)
