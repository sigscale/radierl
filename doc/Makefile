## $Id$

UNAME := $(shell uname)

ifeq ($(UNAME), SunOS)
	INSTALL=/usr/ucb/install
else
	INSTALL=install
endif

.PHONY=install
install:
ifdef ERL_TOP
	$(INSTALL) -d -o otpuser -g otp -m 0755 \
		$(ERL_TOP)/lib/radius-$(APP_VERSION)
	$(INSTALL) -d -o otpuser -g otp -m 0755 \
		$(ERL_TOP)/lib/radius-$(APP_VERSION)/doc
	(for i in `ls *.html`; do $(INSTALL) \
		-o otpuser -g otp -m 0644 $$i \
		$(ERL_TOP)/lib/radius-$(APP_VERSION)/doc; done)
	(for i in `ls *.png`; do $(INSTALL) \
		-o otpuser -g otp -m 0644 $$i \
		$(ERL_TOP)/lib/radius-$(APP_VERSION)/doc; done)
	$(INSTALL) -o otpuser -g otp -m 0644 stylesheet.css \
		$(ERL_TOP)/lib/radius-$(APP_VERSION)/doc
else
	@echo "You must set the ERL_TOP environment variable."
endif
