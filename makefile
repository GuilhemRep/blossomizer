all:
	ocamlc -c main.ml
	ocamlc -o main main.cmo

clean:
	rm -rf main *.cmi *.cmo *~
