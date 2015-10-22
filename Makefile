BUILD = corebuild

default: build

sack.native: sack.ml
	$(BUILD) sack.native -package core_extended

build: sack.native

install: sack.native
	cp _build/sack.native ~/bin/sack && \
		chmod +x ~/bin/sack

clean:
	$(RM) _build/*
