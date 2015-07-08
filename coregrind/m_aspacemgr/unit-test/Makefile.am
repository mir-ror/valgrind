include $(top_srcdir)/Makefile.all.am

EXTRA_DIST = 

dist_noinst_SCRIPTS = 

#----------------------------------------------------------------------------
# Headers
#----------------------------------------------------------------------------

pkginclude_HEADERS = 
noinst_HEADERS = tree.h

#----------------------------------------------------------------------------
# vbit_test
#----------------------------------------------------------------------------

noinst_PROGRAMS = unit-test

if VGCONF_OS_IS_DARWIN
noinst_DSYMS = $(noinst_PROGRAMS)
endif

SOURCES = \
	main.c \
	stubs.c \
	dot.c

unit_test_SOURCES      = $(SOURCES)
unit_test_CPPFLAGS     = $(AM_CPPFLAGS_PRI) \
			 -DASPACEMGR_UNIT_TEST \
                         -I$(top_srcdir)/include  \
                         -I$(top_srcdir)/coregrind  \
                         -I$(top_srcdir)/coregrind/m_aspacemgr  \
                         -I$(top_srcdir)/VEX/pub
unit_test_CFLAGS       = $(AM_CFLAGS_PRI) -std=c99
unit_test_DEPENDENCIES = 
unit_test_LDADD        = 
unit_test_LDFLAGS      = $(AM_CFLAGS_PRI) -std=c99 @LIB_UBSAN@
