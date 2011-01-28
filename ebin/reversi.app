{application, reversi,
 [
  {description, "Erlang Reversi server (Klarna Programming Contest)"},
  {modules,
   [
    client_handler_sup,
    game_server,
    game_server_sup,
    gen_client_handler,
    lobby,
    rev_bot,
    reversi,
    reversi_app,
    reversi_sup,
    rev_game_db,
    rev_serv,
    servers_sup
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
