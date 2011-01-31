ERL ?= erl
APP := reversi

.PHONY: deps

all: deps
	@./rebar compile

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

# docs:
# 	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'
bootscripts: beams
	erl -pa ./apps/reversi/ebin/ -noshell -eval 'systools:make_script("reversi-0.1", [local]).' -s init stop
