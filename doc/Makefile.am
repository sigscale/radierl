## Makefile.am
##
## Process this file with automake to produce Makefile.in

modhtml = radius.html \
			radius_app.html \
			radius_attributes.html \
			radius_fsm.html \
			radius_fsm_sup.html \
			radius_server.html \
			radius_server_sup.html \
			radius_sup.html

doc_DATA = overview.edoc $(modhtml) \
		modules-frame.html overview-summary.html packages-frame.html \
		index.html stylesheet.css

overview.edoc: overview.edoc-in
	sed -e 's!%APP_VERSION%!$(PACKAGE_VERSION)!' \
			-e 's!%APP_NAME%!$(PACKAGE_NAME)!' < $< > $@

%.html:$(top_srcdir)/src/%.erl	overview.edoc
	$(ERL) -noshell -run edoc_run application \
			"'$(PACKAGE_NAME)'" "'$(top_builddir)'" \
			'[{sort_functions,false},{private,true},{todo,true},{source_path,["$(top_srcdir)/src"]},{subpackages,false},{def,{docsrc,"$(top_srcdir)/doc/{@?}"}},{includes, ["$(top_srcdir)/include"]}]'
	for i in `ls *.html`; do \
			sed -i -e 's!erlang logo!motivity logo!' $$i; \
			sed -i -e 's!erlang.org!motivity.ca!' $$i; \
			sed -i -e 's!erlang.png!motivitylogo.png!' $$i; done
	if [ ! -e $(top_builddir)/doc/motivitylogo.png ]; then \
		cp $(top_srcdir)/doc/motivitylogo.png $(top_builddir)/doc; fi
	if [ ! -e $(top_builddir)/doc/supervision.png ]; then \
		cp $(top_srcdir)/doc/supervision.png $(top_builddir)/doc; fi
	if [ ! -e $(top_builddir)/doc/messages.png ]; then \
		cp $(top_srcdir)/doc/messages.png $(top_builddir)/doc; fi

CLEANFILES = overview.edoc edoc-info index.html \
		modules-frame.html overview-summary.html packages-frame.html \
		stylesheet.css erlang.png $(modhtml)

INSTALL += -o otpuser -g otp

