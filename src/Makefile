MMC = mmc
PARALLEL = -j $(shell nproc 2>/dev/null || echo 1)

files = $(wildcard **.m)

../fhhg: fhhg
	@ln -L -f fhhg ../fhhg

fhhg: $(files) Mercury.options
	@$(MMC) --make $(PARALLEL) $@

tags: $(files)
	@mtags $(files)

.PHONY: clean
clean:
	rm -rf Mercury
	rm -f *.err *.mh fhhg
