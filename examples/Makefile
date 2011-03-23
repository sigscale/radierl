## $Id$

export APP_DESC=RADIUS Example Server
export APP_ID=RADIERL_EXAMPLE
export APP_VERSION=0.1

export ERL=erl

docdir = ./doc

.PHONY=all
all: beams html

.PHONY=beams
beams:
	cd src && $(MAKE) $@

edocs	= $(addprefix $(docdir)/, overview.edoc)

.PHONY=edoc
edoc:	$(edocs)
	
$(docdir)/%.edoc:	$(docdir)/%.edoc-in
	sed -e 's!%VERSION%!$(APP_VERSION)!' \
			-e 's!%DESCRIPTION%!$(APP_DESC)!' < $< > $@

.PHONY=html
html:	edoc
	$(ERL) -noshell -run edoc_run application "'radius_example'" '"."' \
			'[{sort_functions, false}, {private, true}, {todo, true}]'
	@cd doc && (for i in `ls *.html`; do sed -e 's!erlang logo!Motivity logo!' \
			  -e 's!erlang.org!motivity.ca!' \
			  -e 's!erlang.png!motivitylogo.png!' < $$i > t.html; \
				mv t.html $$i; done)

.PHONY=clean
clean:
	cd src && $(MAKE) $@
	-cd doc && rm -f edoc-info index.html modules-frame.html \
			overview-summary.html packages-frame.html \
			stylesheet.css overview.edoc erlang.png
	-cd test && rm -f *.beam
	-cd test/log && rm -rf *.html ct_run.* variables-ct*

.PHONY=check
check:	beams
	dialyzer ebin ../ebin

.PHONY=test
test:	beams
	erl -sname ct -config test/sys.config -pa ebin ../ebin \
			-s ct_run script_start -s erlang halt -dir test -logdir test/log

.PHONY=install_examples
install_examples:	beams html
ifdef EXAMPLES_DIR
	cd src && $(MAKE) $@
	cd doc && $(MAKE) $@
else
	@echo "You must set the EXAMPLES_DIR environment variable."
endif

.PHONY=install
install:	beams html
ifdef ERL_TOP
	cd src && $(MAKE) $@
	cd doc && $(MAKE) $@
else
	@echo "You must set the ERL_TOP environment variable."
endif

