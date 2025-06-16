# Global make targets

.PHONY: all
all: allexe alldata

.PHONY: clean
clean:
	rm build/* || true
	rm data/allNonograms || true
	rm data/allHints || true
	rm data/uniqueHints || true
	rm data/numUniqueHints || true
	rm exe/* || true

# Executable make targets

.PHONY: allexe
allexe: exe/allNonograms exe/makeHints

.PHONY: exedirs
exedirs: build exe

build:
	mkdir build

exe:
	mkdir exe

ghc_command = ghc -i"src" -outputdir build -Wno-tabs -O

exe/allNonograms: src/allNonograms.hs src/Nonogram.hs | exedirs
	$(ghc_command) src/allNonograms.hs -o exe/allNonograms

exe/makeHints: src/makeHints.hs src/Nonogram.hs | exedirs
	$(ghc_command) src/makeHints.hs -o exe/makeHints

# Data make targets

.PHONY: alldata
alldata: data/allNonograms data/allHints data/uniqueHints data/numUniqueHints

data/allNonograms: data/size | exe/allNonograms
	exe/allNonograms < data/size > data/allNonograms

data/allHints: data/allNonograms | exe/makeHints
	exe/makeHints < data/allNonograms > data/allHints

data/uniqueHints: data/allHints
	sort < data/allHints | uniq -u > data/uniqueHints

data/numUniqueHints: data/uniqueHints
	wc -l < data/uniqueHints > data/numUniqueHints