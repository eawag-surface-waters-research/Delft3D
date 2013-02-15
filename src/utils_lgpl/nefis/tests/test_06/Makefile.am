#-------------------------------------------------------------------------------
#
#   Jan.Mooiman@deltares.nl
#   23 oct 2007
#
#   Copyright (C) 2007, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = test_06_dependent

test_06_dependent_SOURCES = \
	test_06.f

test_06_dependent_LDADD = \
	../../packages/nefis/src/libNefis.la \
	$(FCLIBS)
