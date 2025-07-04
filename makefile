# SHELL := /bin/bash

# Global make targets

.PHONY: all
all: exe/all data/all

.PHONY: clean
clean: exe/clean data/clean

.PHONY: fullclean
fullclean: exe/fullclean data/fullclean

# Executable make targets

.PHONY: exe/all
exe/all: exe/allGrids exe/makeHints

.PHONY: exe/clean
exe/clean:
	-rm build/*
	-rm exe/*

.PHONY: exe/fullclean
exe/fullclean: exe/clean
	-rmdir build
	-rmdir exe

ghc_command = ghc -i"src" -outputdir build -Wno-tabs -O

exe/allGrids: src/allGrids.hs src/Nonogram.hs
	-mkdir exe
	-mkdir build
	$(ghc_command) src/allGrids.hs -o exe/allGrids

exe/makeHints: src/makeHints.hs src/Nonogram.hs
	-mkdir exe
	-mkdir build
	$(ghc_command) src/makeHints.hs -o exe/makeHints

# Data make targets

.PHONY: data/all
data/all: $(foreach dir,$(wildcard data/*),$(dir)/all)

.PHONY: data/clean
data/clean: $(foreach dir,$(wildcard data/*),$(dir)/clean)

.PHONY: data/fullclean
data/fullclean: $(foreach dir,$(wildcard data/*),$(dir)/fullclean)
	-rmdir data

.PHONY: data/%/all
data/%/all: \
	data/%/size \
	data/%/allGrids \
	data/%/allHints \
	data/%/uniqueHints \
	data/%/numUniqueHints

	@true

.PHONY: data/%/clean
data/%/clean:
	-rm $(@D)/*

.PHONY: data/%/fullclean
data/%/fullclean: data/%/clean
	-rmdir $(@D)

.NOTINTERMEDIATE: data/%/size
data/%/size:
	mkdir -p $(@D)
	$(eval size = $(subst data/,,$(subst /size,,$@)))
	@if [ $$(grep -Ec "^(0|[1-9][0-9]*)$$"<<< "$(size)") -eq 0 ]; \
	then \
		echo "Cannot automatically determine size, as directory is not a number"; \
		false; \
	fi;
	echo "$(size)" > $@;

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