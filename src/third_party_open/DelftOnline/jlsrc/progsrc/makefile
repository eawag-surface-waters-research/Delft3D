#-------------------------------------------------------------------------------
#   Makefile for JavaLaunch Test Program
#
#   Irv.Elshoff@wldelft.nl
#   11 feb 07
#
#   Copyright (C) 2006-2007, WL | Delft Hydraulics
#-------------------------------------------------------------------------------


TOPDIR	= ..
include $(TOPDIR)/make/preamble.mkf


PROG =	testlaunch

SRC_CPP = \
	testlaunch.cpp \

EXTRA_DEPS = \
	$(TOPDIR)/lib/$(MAKE_PLATFORM)/libJavaLaunch.so \

EXTRA_LIBS = \
	-L$(TOPDIR)/lib/$(MAKE_PLATFORM) -lJavaLaunch \


include $(TOPDIR)/make/postamble-exe.mkf
