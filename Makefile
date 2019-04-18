.PHONY: compile rel test typecheck

REBAR=./rebar3

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

cover:
	$(REBAR) cover

test: compile
	$(REBAR) as test do ct --verbose

typecheck:
	$(REBAR) dialyzer

ci:
	$(REBAR) dialyzer && $(REBAR) as test do ct -c --verbose, cover -v
