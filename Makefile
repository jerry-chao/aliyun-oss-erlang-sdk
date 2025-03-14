.PHONY: all compile clean test dialyzer

REBAR = $(shell which rebar3 || echo ./rebar3)

all: compile

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

test:
	$(REBAR) eunit
	$(REBAR) ct

dialyzer:
	$(REBAR) dialyzer

shell:
	$(REBAR) shell 