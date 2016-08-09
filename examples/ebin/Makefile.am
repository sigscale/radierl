## Makefile.am
##
## Process this file with automake to produce Makefile.in

appdir = $(beamdir)
app_DATA = radius_example.app
EXTRA_DIST = radius_example.app

beam_DATA = radius_example.beam \
		radius_example_accounting.beam \
		radius_example_accounting_sup.beam \
		radius_example_app.beam \
		radius_example_authentication.beam \
		radius_example_authentication_sup.beam \
		radius_example_sup.beam

%.beam:$(top_srcdir)/src/%.erl
	$(ERLC) $(ERLCFLAGS) -I $(ERLANG_LIB_DIR_radius)/include $<

%.app:   %.app-in
	sed -e 's/%APP_NAME%/$(PACKAGE_NAME)/' \
			-e 's/%APP_VERSION%/$(PACKAGE_VERSION)/' < $< > $@

CLEANFILES = $(beam_DATA) $(app_DATA)

