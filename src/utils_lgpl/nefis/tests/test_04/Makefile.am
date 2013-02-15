#-------------------------------------------------------------------------------
#
#   Jan.Mooiman@deltares.nl
#   23 oct 2007
#
#   Copyright (C) 2007, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = test_04_dependent

test_04_dependent_SOURCES = \
	test_04.f

test_04_dependent_LDADD = \
	../../packages/nefis/src/libNefis.la \
	$(FCLIBS)
