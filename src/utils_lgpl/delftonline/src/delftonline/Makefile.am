#-------------------------------------------------------------------------------
#   Makefile for DelftOnline
#
#   Irv.Elshoff@Deltares.NL
#   26 may 12
#-------------------------------------------------------------------------------


include $(top_srcdir)/common.am

lib_LTLIBRARIES = \
    libDelftOnline.la

INCLUDES = \
    $(STANDARD_C_INCLUDES)

libDelftOnline_la_SOURCES = \
    arrayshape.cpp \
    barrier.cpp \
    client.cpp \
    clock.cpp \
    connection.cpp \
    dataelement.cpp \
    dictionary.cpp \
    directory.cpp \
    exception.cpp \
    function.cpp \
    linkedlist.cpp \
    log.cpp \
    print.cpp \
    server.cpp \
    sortedbag.cpp \
    util.cpp


AM_CPPFLAGS = \
    -Wno-int-to-pointer-cast
