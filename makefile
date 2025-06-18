# SHELL := /bin/bash

# Global make targets

.PHONY: all
all: exe/all data/all

.PHONY: clean
clean: exe/clean data/clean

# Executable make targets

.PHONY: exe/all
exe/all: exe/allGrids exe/makeHints

.PHONY: exe/clean
exe/clean:
	rm build/*
	rm exe/*

.PHONY: exedirs
exedirs: | build exe

build:
	mkdir build || true

exe:
	mkdir exe || true

ghc_command = ghc -i"src" -outputdir build -Wno-tabs -O

exe/allGrids: src/allGrids.hs src/Nonogram.hs | exedirs
	$(ghc_command) src/allGrids.hs -o exe/allGrids

exe/makeHints: src/makeHints.hs src/Nonogram.hs | exedirs
	$(ghc_command) src/makeHints.hs -o exe/makeHints

# Data make targets

.PHONY: data/all
data/all: $(foreach dir,$(wildcard data/*),$(dir)/all)

.PHONY: data/clean
data/clean: $(foreach dir,$(wildcard data/*),$(dir)/clean)

.PHONY: data/%/all
data/%/all: data/%/size data/%/allGrids data/%/allHints data/%/uniqueHints data/%/numUniqueHints
	@true

.PHONY: data/%/clean
data/%/clean:
	rm $(@D)/*

data:
	mkdir data || true

.NOTINTERMEDIATE: data/%/
data/%/: | data
	mkdir $@ || true

.NOTINTERMEDIATE: data/%/size
data/%/size: | data/%/
	if [ $$(grep -c "data/[1-9][0-9]*/size"<<< "$@") -eq 1 ]; \
	then \
		echo $(subst data/,,$(subst /size,,$@)) > $@; \
	else \
		echo "Cannot automatically determine size as directory is not a number"; \
		false; \
	fi;

.NOTINTERMEDIATE: data/%/allGrids
data/%/allGrids: data/%/size | exe/allGrids
	exe/allGrids < $< > $@

.NOTINTERMEDIATE: data/%/allHints
data/%/allHints: data/%/allGrids | exe/makeHints
	exe/makeHints < $< > $@

.NOTINTERMEDIATE: data/%/uniqueHints
data/%/uniqueHints: data/%/allHints
	sort < $< | uniq -u > $@

.NOTINTERMEDIATE: data/%/numUniqueHints
data/%/numUniqueHints: data/%/uniqueHints
	wc -l < $< > $@