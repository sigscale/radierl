## Makefile

UNAME := $(shell uname)

ifeq ($(UNAME), SunOS)
	INSTALL=/usr/ucb/install
else
	INSTALL=install
endif

ERLC = erlc
ERLCFLAGS = -v -W -I $(incdir) -o $(beamdir) -pa ../ebin +debug_info

incdir	= ../include
beamdir	= ../ebin
htmldir	= ../doc

incfiles	= $(incdir)/radius.hrl

erlfiles	= radius_app.erl radius_sup.erl radius_server_sup.erl \
			radius_fsm_sup.erl radius_server.erl radius_fsm.erl \
			radius.erl radius_attributes.erl

beamfiles	= $(addprefix $(beamdir)/, $(erlfiles:.erl=.beam))

htmlfiles	= $(addprefix $(htmldir)/, $(erlfiles:.erl=.html))

.PHONY=beams
beams:	$(beamfiles) $(beamdir)/radius.app

$(beamdir)/%.beam:%.erl $(incfiles)
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

.PHONY=install
install:	beams
ifdef ERL_TOP
	$(INSTALL) -d -o otpuser -g otp -m 0755 \
		$(ERL_TOP)/lib/radius-$(APP_VERSION)
	$(INSTALL) -d -o otpuser -g otp -m 0755 \
		$(ERL_TOP)/lib/radius-$(APP_VERSION)/ebin
	(for i in $(beamfiles); do $(INSTALL) \
		-o otpuser -g otp -m 0644 $$i \
		$(ERL_TOP)/lib/radius-$(APP_VERSION)/ebin; done)
	$(INSTALL) -o otpuser -g otp -m 0644 $(beamdir)/radius.app \
		$(ERL_TOP)/lib/radius-$(APP_VERSION)/ebin
	$(INSTALL) -d -o otpuser -g otp -m 0755 \
		$(ERL_TOP)/lib/radius-$(APP_VERSION)/src
	(for i in $(erlfiles); do $(INSTALL) \
		-o otpuser -g otp -m 0644 $$i \
		$(ERL_TOP)/lib/radius-$(APP_VERSION)/src; done)
else
	@echo "You must set the ERL_TOP environment variable."
endif

