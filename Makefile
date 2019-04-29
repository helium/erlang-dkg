.PHONY: compile rel test typecheck

REBAR=./rebar3

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

cover:
	$(REBAR) cover

test: compile
	$(REBAR) as test do ct

typecheck:
	$(REBAR) dialyzer

ci:
	$(REBAR) dialyzer && $(REBAR) as test do ct,cover
	$(REBAR) covertool generate
	codecov -f _build/test/covertool/dkg.covertool.xml
