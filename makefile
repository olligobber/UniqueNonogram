# Global make targets

.PHONY: all
all: exe/all data/all

.PHONY: clean
clean:
	rm build/* || true
	rm data/allGrids || true
	rm data/allHints || true
	rm data/uniqueHints || true
	rm data/numUniqueHints || true
	rm exe/* || true

# Executable make targets

.PHONY: exe/all
exe/all: exe/allGrids exe/makeHints

.PHONY: exedirs
exedirs: | build exe

build:
	mkdir build

exe:
	mkdir exe

ghc_command = ghc -i"src" -outputdir build -Wno-tabs -O

exe/allGrids: src/allGrids.hs src/Nonogram.hs | exedirs
	$(ghc_command) src/allGrids.hs -o exe/allGrids

exe/makeHints: src/makeHints.hs src/Nonogram.hs | exedirs
	$(ghc_command) src/makeHints.hs -o exe/makeHints

# Data make targets

.PHONY: data/all
data/all: data/allGrids data/allHints data/uniqueHints data/numUniqueHints

data/allGrids: data/size | exe/allGrids
	exe/allGrids < data/size > data/allGrids

data/allHints: data/allGrids | exe/makeHints
	exe/makeHints < data/allGrids > data/allHints

data/uniqueHints: data/allHints
	sort < data/allHints | uniq -u > data/uniqueHints

data/numUniqueHints: data/uniqueHints
	wc -l < data/uniqueHints > data/numUniqueHints