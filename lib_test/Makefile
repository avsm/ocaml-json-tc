CAMLP4ORF ?= camlp4orf

test: check
check:
	ocamlfind ocamlc -package json-wheel -linkpkg -o $@ \
	  -pp "$(CAMLP4ORF) -I `ocamlfind query type-conv` -I ../lib pa_type_conv.cma pa_json_tc.cmo" $@.ml
	./$@

clean:
	rm -f *.ppo *.ppr *.cmo *.cmi *.o *.cmx *.ast *~ *.auto *.annot check
