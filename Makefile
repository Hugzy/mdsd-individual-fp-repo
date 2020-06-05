all: clean individual
.PHONY: all

individual:
	ocamlbuild -r \
	-use-ocamlfind \
	-package \
	qcheck,\
	cohttp-lwt-unix,\
	cohttp,\
	lwt,\
	yojson,\
	ppx_deriving.show,\
	ppx_deriving_yojson,\
	qcstm,\
	curl \
	-tag thread \
	individual.native \
	http.native \
	individualexternals.native

play:
	ocamlbuild -r \
	-use-ocamlfind \
	-package \
	qcheck,\
	yojson,\
	ppx_deriving.show,\
	ppx_deriving_yojson,\
	qcstm,\
	curl \
	-tag thread \
	generator.native \

runp:
	./generator.native

run:
	./individual.native

clean:
	ocamlbuild -clean
