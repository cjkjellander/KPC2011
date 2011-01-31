ERL ?= erl
APP := reversi

.PHONY: deps

all: deps
	@./rebar compile

release: all
	@./rebar generate

releaseclean:
	rm -rf rel/reversi

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean releaseclean
	@./rebar delete-deps

# docs:
# 	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'
