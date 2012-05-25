#-------------------------------------------------------------------------------
#   Makefile for DelftOnline
#
#   Irv.Elshoff@Deltares.NL
#   25 may 12
#-------------------------------------------------------------------------------


include $(top_srcdir)/common.am

bin_PROGRAMS = dolcli

dolcli_SOURCES = \
	dolcli.cpp \
        lexan.l \
        parse.y

INCLUDES = \
	-I$(top_builddir)/utils_lgpl/delftonline/include

dolcli_LDADD = \
	-L$(top_builddir)/lib -lDelftOnline \
        -ll \
        -lreadline \
        -ltermcap

	
AM_LFLAGS = -L --c++
AM_YFLAGS = -d


