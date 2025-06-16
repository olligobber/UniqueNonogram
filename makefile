.PHONY: all
all: allexe alldata

.PHONY: allexe
allexe: exe/allNonograms exe/makeHints

.PHONY: alldata
alldata: data/allNonograms data/allHints data/numUniqueHints

.PHONY: exedirs
exedirs:
	@mkdir build 2>/dev/null || true
	@mkdir exe 2>/dev/null || true

ghc_command := ghc -i"src" -outputdir build -Wno-tabs -O

exe/allNonograms: src/allNonograms.hs src/Nonogram.hs exedirs
	$(ghc_command) src/allNonograms.hs -o exe/allNonograms

exe/makeHints: src/makeHints.hs src/Nonogram.hs exedirs
	$(ghc_command) src/makeHints.hs -o exe/makeHints

data/allNonograms: exe/allNonograms data/size
	exe/allNonograms < data/size > data/allNonograms

data/allHints: exe/makeHints data/allNonograms
	exe/makeHints < data/allNonograms > data/allHints

data/uniqueHints: data/allHints
	sort < data/allHints | uniq -u > data/uniqueHints

data/numUniqueHints: data/uniqueHints
	wc -l < data/uniqueHints > data/numUniqueHints

.PHONY: clean
clean:
	rm build/* || true
	rm data/allHints || true
	rm data/allNonograms || true
	rm data/numUniqueHints || true
	rm exe/* || true