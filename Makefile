
REBAR_OPTS = $(shell test "X$$NDEBUG" = "X" || echo "-D no_debug")
REBAR = ./rebar $(REBAR_OPTS)

compile: get-deps
	$(REBAR) compile

get-deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean

run:
	erl -pa ebin -pa deps/*/ebin
