#-------------------------------------------------------------------------------
#
#   Jan.Mooiman@deltares.nl
#   23 oct 2007
#
#   Copyright (C) 2007, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = test_00

#-------------------------------------------------------------------------------
#Includes
INCLUDES = -I../../packages/nefis/include

test_00_SOURCES = \
	test_00.c
test_00_LDADD = \
	../../packages/nefis/src/libNefis.la \
	$(CCLIBS)
