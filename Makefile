# Copyright © Marc Nieper-Wißkirchen (2023).

libdirs = lib:scheme-libraries/lib:scheme-libraries/x86_64-linux-gnu/lib
scheme = scheme --libdirs $(libdirs)
scheme-script = $(scheme) --program
prove = prove --exec '$(scheme-script)' --ext '.sps' --failures

all:

check:
	$(prove) tests

cartan:
	@$(scheme-script) cartan.sps

repl:
	@$(scheme) schemerc

.PHONY: all check repl
