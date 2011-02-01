ERL ?= erl
APP := reversi
REVERSI_NODE = reversi@127.0.0.1
ERTS_VERSION = 5.7.5

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

schema:
	erl -noshell -name $(REVERSI_NODE) -eval 'mnesia:create_schema([node()]).' -s init stop

tables: schema release
	rel/reversi/erts-$(ERTS_VERSION)/bin/erl -noshell -name $(REVERSI_NODE) -s mnesia -eval 'reversi_app:create_tables()' -s init stop

# docs:
# 	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'
