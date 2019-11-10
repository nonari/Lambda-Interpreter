ast: ast.ml
	ocamlc -c $<

lexer.ml: lexer.mll
	ocamllex $<

parser.ml: parser.mly
	ocamlyacc $<

parser.i: parser.mli
	ocamlc -c $<

parser: parser.ml parser.i
	ocamlc -c $<

lexer: lexer.ml
	ocamlc -c $<

lambda: lambda.ml
	ocamlc -c $<

main: main.ml
	ocamlc -c $<

all: clean ast parser lexer lambda main
	ocamlc -o lambda parser.cmo lexer.cmo lambda.cmo main.cmo

execute:
	./lambda

clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml parser.mli lambda a.out
