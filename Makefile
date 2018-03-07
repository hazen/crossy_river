.PHONY: test

REBAR = rebar3

all: compile

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

test: compile
	$(REBAR) eunit

dialyze: compile
	$(REBAR) dialyzer

rel: compile
	$(REBAR) release
