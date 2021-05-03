.PHONY: compile rel test typecheck

REBAR=./rebar3

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

cover:
	$(REBAR) covertool generate

test: compile
	$(REBAR) as test do eunit,ct

typecheck:
	$(REBAR) dialyzer

ci:
	$(REBAR) dialyzer && $(REBAR) as test do ct,cover
	$(REBAR) covertool generate
	codecov -f _build/test/covertool/dkg.covertool.xml
