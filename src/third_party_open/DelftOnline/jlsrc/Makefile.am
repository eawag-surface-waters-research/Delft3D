SUBDIRS=libsrc
# #-------------------------------------------------------------------------------
# #   Top-level Makefile for JavaLaunch
# #
# #   Irv.Elshoff@wldelft.nl
# #   13 feb 07
# #
# #   Copyright (C) 2006-2007, WL | Delft Hydraulics
# #-------------------------------------------------------------------------------


# ifeq ($(MAKE_PLATFORM),)

# all clean clobber dist distclean install msclean:
# 	@echo "You must run the setup script before making JavaLaunch"

# else

# TOPDIR   = .
# INSTDIR  = ..

# include $(TOPDIR)/make/preamble.mkf

# MAKE_DIRECTORIES = \
# 	libsrc \
# 	progsrc \
# 	

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
# 	install -Dpv  $(TOPDIR)/include/JavaLaunch.h $(INSTDIR)/include/JavaLaunch.h
# 	install -Dpsv $(TOPDIR)/lib/$(MAKE_PLATFORM)/libJavaLaunch.so $(INSTDIR)/lib/libJavaLaunch.so
# 	install -Dpsv $(TOPDIR)/bin/$(MAKE_PLATFORM)/testlaunch $(INSTDIR)/bin/testlaunch
# 	install -Dpv  $(TOPDIR)/visualstudio/Release/JavaLaunch.dll $(INSTDIR)/lib/JavaLaunch.dll
# 	install -Dpv  $(TOPDIR)/visualstudio/Release/JavaLaunch.lib $(INSTDIR)/lib/JavaLaunch.lib
# 	install -Dpv  $(TOPDIR)/visualstudio/Release/testlaunch.exe $(INSTDIR)/bin/testlaunch.exe

# dist:
# 	( \
# 	    cd $(INSTDIR); \
# 	    tar czf JavaLaunch.tgz bin include lib; \
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
# 	@rm -rf $(INSTDIR)/{bin,include,lib}

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
