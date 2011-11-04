SUBDIRS=include libsrc

# #-------------------------------------------------------------------------------
# #   Top-level Makefile for DelftOnline
# #
# #   Irv.Elshoff@wldelft.nl
# #   13 feb 07
# #
# #   Copyright (C) 2006-2007, WL | Delft Hydraulics
# #-------------------------------------------------------------------------------


# ifeq ($(MAKE_PLATFORM),)

# all clean clobber dist distclean install msclean:
# 	@echo "You must run the setup script before making DelftOnline"

# else

# TOPDIR   = .
# INSTDIR  = ..

# include $(TOPDIR)/make/preamble.mkf

# MAKE_DIRECTORIES = \
# 	libsrc \
# 	progsrc/doltest \
# 	progsrc/printcontents 

# DISABLED = 

# all clean clobber:
# 	@for dir in $(MAKE_DIRECTORIES); do \
# 	    cd $(CURDIR)/$$dir ; \
# 	    $(MAKE) $@ ; exitcode=$$? ; \
# 	    \
# 	    if [ $$exitcode -ne 0 -a "$$MAKE_CONTINUE" = 'NO' ]; then \
# 		echo Exiting make because of problem in `/bin/pwd` ; \
# 		exit $$exitcode ; \
# 	    fi ; \
# 	done

# install: all
# 	install -Dpv  $(TOPDIR)/include/DelftOnline.h $(INSTDIR)/include/DelftOnline.h
# 	install -Dpv  $(TOPDIR)/include/DelftOnline.i $(INSTDIR)/include/DelftOnline.i
# 	install -Dpsv $(TOPDIR)/lib/$(MAKE_PLATFORM)/libDelftOnline.so $(INSTDIR)/lib/libDelftOnline.so
# 	install -Dpsv $(TOPDIR)/lib/$(MAKE_PLATFORM)/libDelftOnlineJNI.so $(INSTDIR)/lib/libDelftOnlineJNI.so
# 	install -Dpsv $(TOPDIR)/bin/$(MAKE_PLATFORM)/doltest $(INSTDIR)/bin/doltest
# 	install -Dpsv $(TOPDIR)/bin/$(MAKE_PLATFORM)/printcontents $(INSTDIR)/bin/printcontents
# 	install -Dpv  $(TOPDIR)/classes/DelftOnline.jar $(INSTDIR)/classes/DelftOnline.jar
# 	install -Dpv  $(TOPDIR)/classes/gnu.getopt.jar $(INSTDIR)/classes/gnu.getopt.jar
# 	install -Dpv  $(TOPDIR)/classes/jline.jar $(INSTDIR)/classes/jline.jar
# 	install -Dpv  $(TOPDIR)/visualstudio/Release/DelftOnline.dll $(INSTDIR)/lib/DelftOnline.dll
# 	install -Dpv  $(TOPDIR)/visualstudio/Release/DelftOnline.lib $(INSTDIR)/lib/DelftOnline.lib
# 	install -Dpv  $(TOPDIR)/visualstudio/Release/DelftOnlineJNI.dll $(INSTDIR)/lib/DelftOnlineJNI.dll
# 	install -Dpv  $(TOPDIR)/visualstudio/Release/DelftOnlineJNI.lib $(INSTDIR)/lib/DelftOnlineJNI.lib

# dist:
# 	( \
# 	    cd $(INSTDIR); \
# 	    tar czf DelftOnline.tgz bin classes include lib; \
# 	)

# distclean:
# 	@$(MAKE) clobber
# 	@$(MAKE) msclean
# 	@rm -rf bin classes java/classes lib modules
# 	@rm -f hs_err_pid*.log
# 	@find . -type f -name \*.bck                            | xargs rm -f
# 	@for f in $(TOPDIR)/make/platform/*.mkf; do \
# 	    platform=`basename $$f .mkf` ; \
# 	    find $(MAKE_DIRECTORIES) -type d -name $$platform | xargs rm -rf ; \
# 	done
# 	@rm -rf $(INSTDIR)/{bin,classes,include,lib}

# msclean:
# 	@find visualstudio -type d -name Debug                  | xargs rm -rf
# 	@find visualstudio -type d -name Release                | xargs rm -rf
# 	@find visualstudio -type f -name \*.lst                 | xargs rm -f
# 	@find visualstudio -type f -name \*.ncb                 | xargs rm -f
# 	@find visualstudio -type f -name \*.obj                 | xargs rm -f
# 	@find visualstudio -type f -name \*.PDB                 | xargs rm -f
# 	@find visualstudio -type f -name \*.plg                 | xargs rm -f
# 	@find visualstudio -type f -name \*.scc                 | xargs rm -f
# 	@find visualstudio -type f -name \*.vcproj.\*.user      | xargs rm -f
# 	@chmod 644 visualstudio/*

# endif
