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
		modules-frame.html overview-summary.html packages-frame.html \
		index.html stylesheet.css

overview.edoc: overview.edoc-in
	sed -e 's!%APP_VERSION%!$(PACKAGE_VERSION)!' \
			-e 's!%APP_NAME%!$(PACKAGE_NAME)!' < $< > $@

%.html:$(top_srcdir)/src/%.erl	overview.edoc
	$(ERL) -noshell -run edoc_run application \
			"'$(PACKAGE_NAME)'" "'$(top_builddir)'" \
			'[{sort_functions,false},{private,true},{todo,true},{source_path,["$(top_srcdir)/src"]},{subpackages,false},{def,{docsrc,"$(top_srcdir)/doc/{@?}"}},{includes,["$(ERLANG_LIB_DIR_radius)/include"]}]'
	for i in `ls *.html`; do sed -e 's!erlang logo!Motivity logo!' \
			-e 's!erlang.org!motivity.ca!' \
			-e 's!erlang.png!motivitylogo.png!' < $$i > t.html; \
					mv t.html $$i; done

CLEANFILES = overview.edoc edoc-info index.html \
		modules-frame.html overview-summary.html packages-frame.html \
		stylesheet.css erlang.png $(modhtml)

