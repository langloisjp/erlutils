.PHONY: deps test dialyzer clean distclean doc compile

all: compile

deps:
	@rebar get-deps

compile: deps
	@rebar compile

test: compile
	@rebar eunit skip_deps=true

dialyzer: all
	@dialyzer -Wrace_conditions -Wunderspecs -r ebin

doc:
	@rebar doc skip_deps=true

clean:
	@rebar clean

distclean: clean
	@rebar delete-deps
