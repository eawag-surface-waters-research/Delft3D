#-------------------------------------------------------------------------------
#   Makefile for DelftOnline Test Program
#
#   Irv.Elshoff@wldelft.nl
#   11 feb 07
#
#   Copyright (C) 2007, WL | Delft Hydraulics
#-------------------------------------------------------------------------------


TOPDIR	= ../..
include $(TOPDIR)/make/preamble.mkf


PROG =	doltest

SRC_CPP = \
	doltest.cpp \
	client.cpp \
	server.cpp \

EXTRA_DEPS = \
	$(TOPDIR)/lib/$(MAKE_PLATFORM)/libDelftOnline.so \

EXTRA_LIBS = \
	-L$(TOPDIR)/lib/$(MAKE_PLATFORM) -lDelftOnline \
	-L$(LIB_JAVALAUNCH)/lib -lJavaLaunch \
	$(LIB_PTHREAD) \
	-lcrypto \


include $(TOPDIR)/make/postamble-exe.mkf
