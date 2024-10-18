.PHONY: hpny
hpny:
	@$(MAKE) -C src ../fhhg

.PHONY: test
test:
	@$(MAKE) -C test test

.PHONY: gen
gen:
	@$(MAKE) -C test gen

.PHONY: clean
clean:
	rm -f fhhg
	@$(MAKE) -C src clean
	@$(MAKE) -C test clean
