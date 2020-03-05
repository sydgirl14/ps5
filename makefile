all: ps5 ps52

ps5: orderedcoll.ml
	ocamlbuild -use-ocamlfind orderedcoll.byte

ps52: prioqueue.ml
		ocamlbuild -use-ocamlfind prioqueue.byte

# expression_tests: expression_tests.ml
# 	ocamlbuild -use-ocamlfind expression_tests.byte

clean:
	rm -rf _build *.byte
