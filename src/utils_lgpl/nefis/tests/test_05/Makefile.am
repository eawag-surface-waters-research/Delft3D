#-------------------------------------------------------------------------------
#
#   Jan.Mooiman@deltares.nl
#   23 oct 2007
#
#   Copyright (C) 2007, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = test_05_dependent

test_05_dependent_SOURCES = \
	test_05.f

test_05_dependent_LDADD = \
	../../packages/nefis/src/libNefis.la \
	$(FCLIBS)
