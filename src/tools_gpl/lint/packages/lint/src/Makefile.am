include $(top_srcdir)/common.am
bin_PROGRAMS = lint

lint_SOURCES = \
	main.cpp

#-------------------------------------------------------------------------------
#Libraries 

lint_LDADD = \
	$(top_srcdir)/tools_gpl/lint/packages/lint_f/src/liblint_f.a \
	$(top_srcdir)/tools_gpl/lint/packages/lint_version_number/src/liblint_version_number.a \
	-ldl \
	$(FCLIBS) -lifcore -limf
