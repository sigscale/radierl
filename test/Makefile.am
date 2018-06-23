## Makefile.am
##
## Process this file with automake to produce Makefile.in

testmods = radius_test_lib \
		radius_test_callback \
		radius_api_SUITE \
		radius_attributes_SUITE

check_DATA = .dialyzer_ok .common_test_ok

.dialyzer_ok:	$(addsuffix .beam, $(testmods))
	dialyzer --src $(top_srcdir)/src

%.beam:$(top_srcdir)/test/%.erl $(top_srcdir)/include/radius.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include $<

sys.config:	$(top_srcdir)/test/sys.config
	cp $< $@

test.spec:	$(top_srcdir)/test/test.spec
	cp $< $@

ct.cfg:	$(top_srcdir)/test/ct.cfg
	cp $< $@

.common_test_ok:	$(addsuffix .beam, $(testmods)) sys.config ct.cfg test.spec
	@if [ ! -d log ]; then mkdir log; fi
	for i in $(addsuffix _data,$(testmods)); do \
		if [ -d $(top_srcdir)/test/$$i ]; then \
			if [ ! -d $$i ]; then \
				mkdir $$i; \
			fi; \
			for j in `ls $(top_srcdir)/test/$$i`; do \
				if [ $(top_srcdir)/test/$$i/$$j -nt $$i/$$j ]; then \
					cp $(top_srcdir)/test/$$i/$$j $$i/$$j; \
				fi; \
			done; \
		fi; \
	done
	ct_run -no_auto_compile -pa $(top_builddir)/ebin -logdir log -config ct.cfg -spec test.spec -dir . -erl_args -config sys -sname $(LOGNAME)-ct

clean-local:
	-rm -r *.beam log/*

