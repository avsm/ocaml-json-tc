include Camlp4Version

VERSION = 0.9.8
export VERSION


.PHONY: default all opt init common check test install uninstall
.PHONY: clean meta doc archive demo

default: jtc all init
all: init common

jtc:
	ocamlfind ocamlc -package type-conv -c -pp "$(CAMLP4ORF)" -annot -g -I +camlp4 pa_json_tc.ml

jtc-%:
	ocamlfind ocamlc -package json-wheel -linkpkg -pp "$(CAMLP4ORF) -I `ocamlfind query type-conv` pa_type_conv.cmo pa_json_tc.cmo" $*.ml

init:
	echo $$CAMLP4_VERSION
	echo $$CAMLP4ORF
	echo $$CAMLP4RF
	echo $$PR_O
	echo $$PR_R
	echo $$PARSER
	echo $$PRINTER
	echo '# 1 "pa_json_static.ml.$(CAMLP4_VERSION)"' > pa_json_static.ml
	cat pa_json_static.ml.$(CAMLP4_VERSION) >> pa_json_static.ml
	ln -sf pa_json_static.ml.annot pa_json_static.annot

common:
	ocamlc -c -dtypes \
		-pp '$(CAMLP4ORF) -loc _loc' \
		-I +camlp4 pa_json_static.ml

demo: yahoo
	./yahoo "Nelson Mandela"

test: check
check:
	camlp4o -I . $(PR_O) $(PARSER) pa_json_static.cmo check.ml -o check.ppo
#	ocamlfind ocamlopt -c -package json-wheel -impl check.ppo
	ocamlfind ocamlc -i -package json-wheel \
		-pp 'camlp4o -I . $(PARSER) pa_json_static.cmo' check.ml \
		> check.mli.auto
	ocamlfind ocamlopt -o check -package json-wheel -linkpkg \
		-pp 'camlp4o -I . $(PARSER) pa_json_static.cmo' \
		check.ml
	./check
install: META
	ocamlfind install json-static META \
		pa_json_static.cmi pa_json_static.cmo pa_json_tc.cmi pa_json_tc.cmo
META: META.template Makefile
	echo 'version = "$(VERSION)"' > META
	cat META.template >> META

uninstall:
	ocamlfind remove json-static
clean:
	rm -f *.ppo *.ppr *.cmo *.cmi *.o *.cmx *.ast *~ *.auto *.annot \
		check yahoo yahoo.ml.html test_typedefs.json \
		pa_json_static.ml

yahoo: yahoo.ml
	ocamlfind ocamlopt -o yahoo yahoo.ml -dtypes -linkpkg \
	  -package json-static,netclient -syntax camlp4o

archive:
	rm -rf /tmp/json-static /tmp/json-static-$(VERSION) && \
	 	cp -r . /tmp/json-static && \
		cd /tmp/json-static && \
			$(MAKE) clean && \
			rm -f *~ json-static*.tar* && \
		cd /tmp && cp -r json-static json-static-$(VERSION) && \
		tar czf json-static.tar.gz json-static && \
		tar cjf json-static.tar.bz2 json-static && \
		tar czf json-static-$(VERSION).tar.gz json-static-$(VERSION) && \
		tar cjf json-static-$(VERSION).tar.bz2 json-static-$(VERSION)
	mv /tmp/json-static.tar.gz /tmp/json-static.tar.bz2 .
	mv /tmp/json-static-$(VERSION).tar.gz /tmp/json-static-$(VERSION).tar.bz2 .
	cp json-static.tar.gz json-static.tar.bz2 $$WWW/
	cp json-static-$(VERSION).tar.gz json-static-$(VERSION).tar.bz2 $$WWW/
	cp LICENSE $$WWW/json-static-license.txt
	cp README $$WWW/json-static-readme.txt
	cp Changes $$WWW/json-static-changes.txt
	cp yahoo.ml $$WWW/
	echo 'let json_static_version = "$(VERSION)"' \
		> $$WWW/json-static-version.ml
