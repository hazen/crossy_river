.PHONY: test

REBAR = rebar3

all: compile

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

test: compile
	$(REBAR) eunit

rel: compile
	$(REBAR) release
