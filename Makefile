SRC=src

OC=ocamlbuild -I $(SRC) -use-ocamlfind

all: main

main: main.native
	mv main.native qcc

main.native:
	$(OC) $@

clean:
	rm -rf _build qcc

.PHONY: all main.native clean
