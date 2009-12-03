VERSION = 0.9.0
export VERSION

CAMLP4ORF ?= camlp4orf

.PHONY: default all opt init common check test install uninstall
.PHONY: clean meta doc archive demo

default: all init
all: init common

all:
	ocamlfind ocamlc -package type-conv -c -pp "$(CAMLP4ORF)" -annot -g -I +camlp4 pa_json_tc.ml

install: META
	ocamlfind install json-tc META pa_json_tc.cmi pa_json_tc.cmo

META: META.template Makefile
	echo 'version = "$(VERSION)"' > META
	cat META.template >> META

uninstall:
	ocamlfind remove json-tc

clean:
	rm -f *.ppo *.ppr *.cmo *.cmi *.o *.cmx *.ast *~ *.auto *.annot META
