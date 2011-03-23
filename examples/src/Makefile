## $Id$

UNAME := $(shell uname)

ifeq ($(UNAME), SunOS)
	INSTALL=/usr/ucb/install
else
	INSTALL=install
endif

ERLC = erlc
ERLCFLAGS = -v -W -I $(incdir) -o $(beamdir) -pa ../../ebin +debug_info

incdir	= ../../include 
beamdir	= ../ebin
htmldir	= ../doc

incfiles	= 

erlfiles	= radius_example_app.erl radius_example_sup.erl \
			radius_example_authentication_sup.erl \
			radius_example_accounting_sup.erl \
			radius_example_authentication.erl radius_example_accounting.erl \
			radius_example.erl

beamfiles	= $(addprefix $(beamdir)/, $(erlfiles:.erl=.beam))

htmlfiles   = $(addprefix $(htmldir)/, $(erlfiles:.erl=.html))

.PHONY=all
all: beams

.PHONY=beams
beams:	$(beamfiles) $(beamdir)/radius_example.app

$(beamdir)/%.beam:%.erl	$(incfiles)
	$(ERLC) $(ERLCFLAGS) $<

$(beamdir)/%.app:	%.app-in
	sed -e 's!%DESC%!$(APP_DESC)!' \
			-e 's!%ID%!$(APP_ID)!' \
			-e 's!%VERSION%!$(APP_VERSION)!' < $< > $@

.PHONY=clean
clean:
	-rm -f $(beamdir)/radius.app
	-rm -f $(beamfiles)
	-rm -f $(htmlfiles)

.PHONY=install_examples
install_examples: beams
ifdef EXAMPLES_DIR
	$(INSTALL) -d -o otpuser -g otp -m 0755 $(EXAMPLES_DIR)
	$(INSTALL) -d -o otpuser -g otp -m 0755 $(EXAMPLES_DIR)/ebin
	(for i in $(beamfiles); do \
		$(INSTALL) -o otpuser -g otp -m 0644 $$i $(EXAMPLES_DIR)/ebin; done)
	$(INSTALL) -o otpuser -g otp -m 0644 $(beamdir)/radius_example.app \
		$(EXAMPLES_DIR)/ebin
	$(INSTALL) -d -o otpuser -g otp -m 0755 $(EXAMPLES_DIR)/src
	(for i in $(erlfiles); do \
		$(INSTALL) -o otpuser -g otp -m 0644 $$i $(EXAMPLES_DIR)/src; done)
else
	@echo "You must set the EXAMPLES_DIR environment variable."
endif

.PHONY=install
install:	beams
ifdef ERL_TOP
	$(INSTALL) -d -o otpuser -g otp -m 0755 \
		$(ERL_TOP)/lib/radius_example-$(APP_VERSION)
	$(INSTALL) -d -o otpuser -g otp -m 0755 \
		$(ERL_TOP)/lib/radius_example-$(APP_VERSION)/ebin
	(for i in $(beamfiles); do $(INSTALL) \
		-o otpuser -g otp -m 0644 $$i \
		$(ERL_TOP)/lib/radius_example-$(APP_VERSION)/ebin; done)
	$(INSTALL) -o otpuser -g otp -m 0644 $(beamdir)/radius_example.app \
		$(ERL_TOP)/lib/radius_example-$(APP_VERSION)/ebin
	$(INSTALL) -d -o otpuser -g otp -m 0755 \
		$(ERL_TOP)/lib/radius_example-$(APP_VERSION)/src
	(for i in $(erlfiles); do $(INSTALL) \
		-o otpuser -g otp -m 0644 $$i \
		$(ERL_TOP)/lib/radius_example-$(APP_VERSION)/src; done)
else
	@echo "You must set the ERL_TOP environment variable."
endif

