ACLOCAL_AMFLAGS=-I m4
# custom location of include
# dol depends on javalaunch
AM_CPPFLAGS = -I. -I$(top_srcdir)/dolsrc/include -I$(top_srcdir)/jlsrc/include 
lib_LTLIBRARIES = libDelftOnline.la
libDelftOnline_la_SOURCES = \
        client.cpp \
        online.cpp \
        print.cpp \
        server.cpp 
# These should be installed
libDelftOnline_la_LDFLAGS = -version-info 0:1:0


# #-------------------------------------------------------------------------------
# #   Makefile for DelftOnline
# #
# #   Irv.Elshoff@wldelft.nl
# #   12 feb 07
# #
# #   Copyright (C) 2006-2007, WL | Delft Hydraulics
# #-------------------------------------------------------------------------------


# TOPDIR  = ..
# include $(TOPDIR)/make/preamble.mkf


# LIB =   libDelftOnline.so

# SRC_CPP = \
#         client.cpp 
#         online.cpp 
#         print.cpp 
#         server.cpp 

# OTHER_TARGETS = \
# 	$(LIBDIR)/libDelftOnlineJNI.so


# $(LIBDIR)/libDelftOnlineJNI.so: $(MAKE_PLATFORM)/jni.o $(MAKE_PLATFORM)/server.o $(MAKE_PLATFORM)/online.o
# 	@echo Creating and installing shared library $(subst $(TOPDIR)/,,$(LIBDIR))/`basename $@`
# 	@mkdir -p $(LIBDIR)
# 	@rm -f $(LIBDIR)/`basename $@`
# 	@if [ "$$MAKE_VERBOSE" = 'YES' ]; then \
# 	    echo $(MAKE_LD) $(MAKE_LDFLAGS) -shared -o $@ $^ ; \
# 	fi
# 	@$(MAKE_LD) $(MAKE_LDFLAGS) -shared -o $@ $^

# $(MAKE_PLATFORM)/jni.o: $(TOPDIR)/include/DelftOnline.h


# include $(TOPDIR)/make/postamble-dll.mkf

