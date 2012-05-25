#-------------------------------------------------------------------------------
#   DelftOnline -- Interactive Client Program
#   Makefile
#
#   Irv.Elshoff@Deltares.NL
#   4 may 12
#
#   Copyright (C) 2006-2012, WL | Deltares
#-------------------------------------------------------------------------------


TOPDIR	= ../..
include $(TOPDIR)/make/preamble.mkf


PROG =	dolcli

SRC_CPP = \
        dolcli.cpp \

SRC_FLEX = \
        lexan.l \

SRC_BISON = \
        parse.y \

EXTRA_DEPS = \
        $(TOPDIR)/lib/$(MAKE_PLATFORM)/libDelftOnline.so \

EXTRA_LIBS = \
        -L$(TOPDIR)/lib/$(MAKE_PLATFORM) -lDelftOnline \
        $(LIB_PTHREAD) \
        -ll \
        -lreadline \
        -ltermcap \

EXTRA_CLEANUP = \
    parse.h \


include $(TOPDIR)/make/postamble-exe.mkf

