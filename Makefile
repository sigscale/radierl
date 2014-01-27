
export APP_DESC=RADIUS Protocol Stack
export APP_ID=RADIERL
export APP_VERSION=1.0

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
	$(ERL) -noshell -run edoc_run application "'radius'" '"."' \
			'[{sort_functions, false}, {private, true}, {todo, true}]'
	@cd doc && (for i in `ls *.html`; do sed -e 's!erlang logo!Motivity logo!' \
			  -e 's!erlang.org!motivity.ca!' \
			  -e 's!erlang.png!motivitylogo.png!' < $$i > t.html; \
				mv t.html $$i; done)

.PHONY=example_callbacks
example_callbacks:
	cd examples && $(MAKE)

.PHONY=clean
clean:
	cd src && $(MAKE) $@
	cd examples && $(MAKE) $@
	-cd doc && rm -f edoc-info index.html modules-frame.html \
			overview-summary.html packages-frame.html \
			stylesheet.css overview.edoc erlang.png
	-cd test && rm -f *.beam
	-cd test/log && rm -rf *.html *.css *.js ct_run.* variables-ct* ct_log_cache 

.PHONY=check
check:	beams
	dialyzer ebin
	cd examples && $(MAKE) $@

.PHONY=test
test:	beams
	erl -sname ct -config test/sys.config -pa ebin \
			-s ct_run script_start -s erlang halt -dir test -logdir test/log
	cd examples && $(MAKE) $@

.PHONY=install
install:	beams html
ifdef ERL_TOP
	cd src && $(MAKE) $@
	cd include && $(MAKE) $@
	cd doc && $(MAKE) $@
	cd examples && EXAMPLES_DIR=$(ERL_TOP)/lib/radius-$(APP_VERSION)/examples \
			$(MAKE) install_examples
else
	@echo "You must set the ERL_TOP environment variable."
endif

