{application, reversi,
 [
  {description, "Erlang Reversi server (Klarna Programming Contest)"},
  {vsn, "0.3"},
  {modules,
   [
    bot,
    client_handler_sup,
    client_handler_tcp,
    game_server,
    game_server_sup,
    gen_client_handler,
    lobby,
    lobby_db,
    bot_base,
    rand_bot2,
    rand_bot_sup,
    rev_bot,
    reversi,
    reversi_app,
    reversi_sup,
    rev_game_db,
    servers_sup,
    tcp_parse
   ]},
  {registered,
   [
    reversi_supervisor,
    reversi_servers_supervisor,
    reversi_game_server_supervisor,
    reversi_client_handler_supervisor,
    reversi_lobby,
    reversi_game_db
   ]},
  {applications,
   [
    kernel,
    stdlib,
    mnesia,
    crypto
   ]},
  {mod, {reversi_app, []}}
 ]}.
