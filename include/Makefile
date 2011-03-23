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
		$(ERL_TOP)/lib/radius-$(APP_VERSION)/include
	$(INSTALL) -o otpuser -g otp -m 0644 radius.hrl \
		$(ERL_TOP)/lib/radius-$(APP_VERSION)/include
else
	@echo "You must set the ERL_TOP environment variable."
endif
