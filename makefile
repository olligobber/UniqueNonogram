all: exe/allNonograms exe/makeHints data/allNonograms data/allHints data/numUniqueHints

ghc_command := ghc -i"src" -hidir build -odir build -Wno-tabs

exe/allNonograms: src/allNonograms.hs src/Nonogram.hs
	$(ghc_command) src/allNonograms.hs -o exe/allNonograms

exe/makeHints: src/makeHints.hs src/Nonogram.hs
	$(ghc_command) src/makeHints.hs -o exe/makeHints

data/allNonograms: exe/allNonograms data/size
	exe/allNonograms < data/size > data/allNonograms

data/allHints: exe/makeHints data/allNonograms
	exe/makeHints < data/allNonograms > data/allHints

data/numUniqueHints: data/allHints
	sort < data/allHints | uniq -u | wc -l > data/numUniqueHints

clean:
	rm build/* | true
	rm data/allHints | true
	rm data/allNonograms | true
	rm data/numUniqueHints | true
	rm exe/* | true