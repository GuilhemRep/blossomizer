all:
	ocamlc -c blossomizer.ml
	ocamlc -o blossomizer blossomizer.cmo

clean:
	rm -rf main *.cmi *.cmo *~
