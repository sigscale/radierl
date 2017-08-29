## Makefile.am
##
## Process this file with automake to produce Makefile.in

modhtml = radius_example.html \
		radius_example_accounting.html \
		radius_example_accounting_sup.html \
		radius_example_app.html \
		radius_example_authentication.html \
		radius_example_authentication_sup.html \
		radius_example_sup.html

doc_DATA = overview.edoc $(modhtml) \
		modules-frame.html overview-summary.html \
		index.html stylesheet.css

overview.edoc: overview.edoc-in
	sed -e 's!%APP_VERSION%!$(PACKAGE_VERSION)!' \
			-e 's!%APP_NAME%!$(PACKAGE_NAME)!' < $< > $@

%.html:$(top_srcdir)/src/%.erl	overview.edoc
	$(ERL) -noshell -run edoc_run application \
			"'$(PACKAGE_NAME)'" "'$(top_builddir)'" \
			'[{sort_functions,false},{private,true},{todo,true},{source_path,["$(top_srcdir)/src"]},{subpackages,false},{def,{docsrc,"$(top_srcdir)/doc/{@?}"}},{includes,["$(ERLANG_LIB_DIR_radius)/include"]}]'
	for i in `ls *.html`; do sed -e 's!erlang logo!Motivity logo!' \
			-e 's!erlang.org!sigscale.org!' \
			-e 's!erlang.png!sigscalelogo.png!' < $$i > t.html; \
					mv t.html $$i; done
	if [ ! -e $(top_builddir)/doc/sigscalelogo.png ]; then \
		cp $(top_srcdir)/doc/sigscalelogo.png $(top_builddir)/doc; fi
	if [ ! -e $(top_builddir)/doc/supervision.png ]; then \
		cp $(top_srcdir)/doc/supervision.png $(top_builddir)/doc; fi
	if [ ! -e $(top_builddir)/doc/callbacks-init ]; then \
		cp $(top_srcdir)/doc/callbacks-init.png $(top_builddir)/doc; fi
	if [ ! -e $(top_builddir)/doc/callbacks-request.png ]; then \
		cp $(top_srcdir)/doc/callbacks-request.png $(top_builddir)/doc; fi
	if [ ! -e $(top_builddir)/doc/callbacks-terminate.png ]; then \
		cp $(top_srcdir)/doc/callbacks-terminate.png $(top_builddir)/doc; fi

CLEANFILES = overview.edoc edoc-info index.html \
		modules-frame.html overview-summary.html \
		stylesheet.css erlang.png $(modhtml)


