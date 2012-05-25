#-------------------------------------------------------------------------------
#   Makefile for DelftOnline
#
#   Irv.Elshoff@Deltares.NL
#   25 may 12
#-------------------------------------------------------------------------------


include $(top_srcdir)/common.am

bin_PROGRAMS = printcontents

printcontents_SOURCES = \
	printcontents.cpp

INCLUDES = \
	-I$(top_builddir)/utils_lgpl/delftonline/include

printcontents_LDADD = \
	-L$(top_builddir)/lib -lDelftOnline

