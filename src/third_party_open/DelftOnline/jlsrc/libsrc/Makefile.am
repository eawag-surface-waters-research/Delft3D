ACLOCAL_AMFLAGS=-I m4
# custom location of include
AM_CPPFLAGS = -I. -I$(top_srcdir)/jlsrc/include 
lib_LTLIBRARIES = libJavaLaunch.la
libJavaLaunch_la_SOURCES = \
	JavaLaunch.cpp
# These should be installed
libJavaLaunch_la_LDFLAGS = -version-info 0:1:0

# #-------------------------------------------------------------------------------
# #   Makefile for JavaLaunch
# #
# #   Irv.Elshoff@wldelft.nl
# #   07 nov 06
# #
# #   Copyright (C) 2006, WL | Delft Hydraulics
# #-------------------------------------------------------------------------------


# TOPDIR  = ..
# include $(TOPDIR)/make/preamble.mkf


# LIB =   libJavaLaunch.so

# SRC_CPP = \
#         JavaLaunch.cpp 


# $(LIBDIR)/libjni.so: $(MAKE_PLATFORM)/jni.o
# 	@echo Creating and installing shared library $(subst $(TOPDIR)/,,$(LIBDIR))/`basename $@`
# 	@mkdir -p $(LIBDIR)
# 	@rm -f $(LIBDIR)/`basename $@`
# 	@if [ "$$MAKE_VERBOSE" = 'YES' ]; then \
# 	    echo $(MAKE_LD) -shared -o $@ $^ ; \
# 	fi
# 	@$(MAKE_LD) -shared -o $@ $^


# include $(TOPDIR)/make/postamble-dll.mkf
