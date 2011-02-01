ERL ?= erl
APP := reversi
REVERSI_NODE = reversi@127.0.0.1
ERTS_VERSION = 5.7.5

.PHONY: deps

all: deps
	@./rebar compile

release: all
	@./rebar generate

releaseschema:
	@cd rel/reversi/ && ./erts-*/bin/erl -noshell -name $(REVERSI_NODE) -eval 'mnesia:create_schema([node()]).' -s mnesia -eval 'reversi_app:create_tables()' -s init stop

releaseclean:
	rm -rf rel/reversi

releaseupgrade: release
	./rebar generate-upgrade previous_release=live
	@echo
	@echo "# copy rel/reversi_Vsn.tar.gz to reversi/releases/ on live"
	@echo "# use release_handler in the running erlang console for the deploy:"
	@echo "erl> release_handler:unpack_release(\"reversi_UpdateVsn\")."
	@echo "erl> release_handler:install_release(\"UpdateVsn\")."
	@echo "erl> release_handler:make_permanent(\"UpdateVsn\")."
	@echo "erl> release_handler:which_releases()."

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean releaseclean
	@./rebar delete-deps

schema:
	erl -noshell -name $(REVERSI_NODE) -eval 'mnesia:create_schema([node()]).' -s init stop

tables: schema
	erl -pa apps/*/ebin -noshell -name $(REVERSI_NODE) -s mnesia -eval 'reversi_app:create_tables()' -s init stop

# docs:
# 	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'
