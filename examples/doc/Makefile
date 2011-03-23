## Makefile

UNAME := $(shell uname)

ifeq ($(UNAME), SunOS)
	INSTALL=/usr/ucb/install
else
	INSTALL=install
endif

.PHONY=install_examples
install_examples:
ifdef EXAMPLES_DIR
	$(INSTALL) -d -o otpuser -g otp -m 0755 $(EXAMPLES_DIR)
	$(INSTALL) -d -o otpuser -g otp -m 0755 $(EXAMPLES_DIR)/doc
	(for i in `ls *.html`; do \
		$(INSTALL) -o otpuser -g otp -m 0644 $$i $(EXAMPLES_DIR)/doc; done)
	(for i in `ls *.png`; do \
		$(INSTALL) -o otpuser -g otp -m 0644 $$i $(EXAMPLES_DIR)/doc; done)
	$(INSTALL) -o otpuser -g otp -m 0644 stylesheet.css $(EXAMPLES_DIR)/doc
else
	@echo "You must set the EXAMPLES_DIR environment variable."
endif

.PHONY=install
install:
ifdef ERL_TOP
	$(INSTALL) -d -o otpuser -g otp -m 0755 \
		$(ERL_TOP)/lib/radius_example-$(APP_VERSION)
	$(INSTALL) -d -o otpuser -g otp -m 0755 \
		$(ERL_TOP)/lib/radius_example-$(APP_VERSION)/doc
	(for i in `ls *.html`; do $(INSTALL) \
		-o otpuser -g otp -m 0644 $$i \
		$(ERL_TOP)/lib/radius_example-$(APP_VERSION)/doc; done)
	(for i in `ls *.png`; do $(INSTALL) \
		-o otpuser -g otp -m 0644 $$i \
		$(ERL_TOP)/lib/radius_example-$(APP_VERSION)/doc; done)
	$(INSTALL) -o otpuser -g otp -m 0644 stylesheet.css \
		$(ERL_TOP)/lib/radius_example-$(APP_VERSION)/doc
else
	@echo "You must set the ERL_TOP environment variable."
endif
