-module(client_handler_tcp).
-behaviour(gen_server).

-export([start_link/1]).

-export([ init/1
          , handle_call/3
          , handle_cast/2
          , handle_info/2
          , terminate/2
          , code_change/3
        ]).

-define(DEFAULT_PORT, 7676).

-define(SOCKET_OPTIONS,
        [
         {active, true},
         {reuseaddr, true},
         {nodelay, true}
        ]).

-record(state, {lsock
                , ip
                , game_server
                , socket
               }).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init([]) ->
    Port = case application:get_env(reversi, port) of
               {ok, P} -> P;
               undefined -> ?DEFAULT_PORT
           end,
    {ok, LSock} = gen_tcp:listen(Port, ?SOCKET_OPTIONS),
    init([LSock]);
init([LSock]) ->
    {ok, #state{lsock = LSock}, 0}.

handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Response, #state{socket=Socket} = State) ->
    NewState = handle_response(Response, Socket, State),
    {noreply, NewState}.

handle_info({tcp, Socket, RawData}, State) ->
    NewState = handle_data(Socket, RawData, State),
    {noreply, NewState};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    {ok, {IP, _Port}} = inet:sockname(Socket),
    {ok, NewCH} = client_handler_sup:start_client_handler(?MODULE, [LSock]),
    gen_tcp:controlling_process(LSock, NewCH),
    {noreply, State#state{ip = IP, socket=Socket}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
handle_data(Socket, RawData, #state{ip = IP, game_server=GS} = State) ->
    Request = tcp_parse:parse_data(RawData),
    case Request of
        [] ->
            %% do nothing
            State;
        {error, could_not_parse_command} ->
            send_msg(Socket, term_to_string(Request)),
            State;
        _ ->
            Response =
                case GS of
                    undefined -> lobby:client_command({Request, IP});
                    _         -> game_server:client_command(GS, Request)
                end,
            handle_response(Response, Socket, State)
    end.

handle_response({redirect, {lets_play, GS, Who, Gid, C}}, Socket, State) ->
    send_msg(Socket, term_to_string({ok, {lets_play, Who, Gid, C}})),
    State#state{game_server=GS};
handle_response({redirect, {game_over, G, Win}}, Socket, State) ->
    send_msg(Socket, term_to_string({ok, {game_over, G, Win}})),
    State#state{game_server=undefined};
handle_response({redirect, {game_crash, G}}, Socket, State) ->
    send_msg(Socket, term_to_string({error, {game_crash, G}})),
    State#state{game_server=undefined};
handle_response(good_bye, Socket, _State) ->
    send_msg(Socket, "good_bye"),
    gen_tcp:close(Socket),
    exit(normal);
handle_response(help_text, Socket, State) ->
    send_msg(Socket, help_text()),
    State;
handle_response(detailed_help_text, Socket, State) ->
    send_msg(Socket, detailed_help_text()),
    State;
handle_response(Response, Socket, State) ->
    send_msg(Socket, term_to_string(Response)),
    State.



term_to_string(Term) ->
    lists:flatten(io_lib:format("~p", [Term])).

send_msg(Socket, Msg) ->
    gen_tcp:send(Socket, Msg ++ "\n").


help_text() ->
    ""
        "LOBBY COMMANDS\n"
        "help. | {help}. -> [This help text]\n"
        "{commands}. -> [More detailed help text]\n"
        "{register,BotName,PlayerName,BotDescription,PlayerEmail}. -> {ok,{password,Password}}\n"
        "{login,BotName,Password}. -> {ok, welcome}\n"
        "{bot_rank,BotName}. -> {ok,BotRank} [Sorry, this command doesn't work just yet]\n"
        "{list_bots}. -> {ok,Bots}\n"
        "{i_want_to_play}. -> {ok,waiting_for_challenge} | {ok,{lets_play,YourColor,GameId,Cookie}}\n"
        "{logout}. -> good_bye\n"
        "\n"
        "GAME COMMANDS\n"
        "{login,Cookie,Color}. -> {ok,{your_move,Game}} | {ok,{please_wait,Game}} | {ok,wait_for_other_guy}\n"
        "{move,Cookie,Color,X,Y}. -> {ok,{please_wait,Game}} | {game_over,GameState,Winner}\n"
        "    Sent after opponent move: {ok,{your_move,Game}} | {game_over,GameState,Winner}\n"
        "{available_moves,Game,Color}. -> {ok,AvailableMoves}\n"
        "{game_status}. -> {ok,GameStatus}\n"
        "{board}. -> {ok,Board}\n"
        "{opponent}. -> {ok,Opponent}\n".

detailed_help_text() ->
    "\n"
        "NOTE 1: All commands must be sent ASCII encoded.\n"
        "NOTE 2: The server will reply with {error, Reason} in case a command is unsuccessful.\n"
        "NOTE 3: All commands to and from the server are terminated by a full stop (.) This means that if you are using erlang you will have to append the full stop to be able to parse a reply from the server.\n"
        "\n"
        "--- Registering ---\n"
        "The first thing you must do is register your bot, this is done by sending:\n"
        "{register, BotName, PlayerName, BotDescription, PlayerEmail}.\n"
        "\n"
        "over telnet to the game server. Upon success you will be logged in and then receive:\n"
        "{ok, {password, Password}}.\n"
        "\n"
        "The password is specific for each bot.\n"
        "\n"
        "--- Login ---\n"
        "To log in, a bot sends:\n"
        "{login, BotName, Password}.\n"
        "\n"
        "which should get reply:\n"
        "{ok, welcome}\n"
        "\n"
        "The bot then may then request a new game from the server by sending:\n"
        "{i_want_to_play}\n"
        "\n"
        "with a reply of either\n"
        "{ok, {lets_play, YourColor, GameId, Cookie}}, where YourColor = 0 (black) or 1 (white) -> There is an available challenger.\n"
        "{ok, waiting_for_challenge} -> There are no available opponents, wait for the above message.\n"
        "\n"
        "The game can then start.\n"
        "\n"
        "--- Game Start ---\n"
        "To enter a game, a bot sends\n"
        "{login, Cookie, Color}.\n"
        "\n"
        "To this the server will reply with one of three results:\n"
        "{ok, {your_move, Game}}   -> Opponent has joined the game, and it is your turn.\n"
        "{ok, {please_wait, Game}} -> Opponent has joined the game, and it is his turn. Wait for the above message from the server.\n"
        "{ok, wait_for_other_guy}  -> Opponent has not joined the game yet, wait for either of the above messages from the server.\n"
        "\n"
        "--- The Game Type Defined ---\n"
        "\n"
        "{ok,{your_move,{game,2076,\"Monkey\",\"Moose\",\n"
        "                    {34628173824,68853694464},\n"
        "                    0,0,[],\n"
        "                    {2,2},\n"
        "                    undefined,undefined,\n"
        "                    {{0,300,0},{0,300,0}}}}}\n"
        "\n"
        "This is an actual game structure returned at the start of the game.\n"
        "The record definition starts with the {game,\n"
        "2076 = GameID that identifies this particular game\n"
        "“Monkey” = login of the black player\n"
        "“Moose” = login of the white player\n"
        "{34628173824,68853694464} = The board, as two 64bit integers in decimal. First the black pieces, then the white. the LSB (bit 0) is the top left corner and the table is defined in rows, so top right is bit 7 (couting from 0), bottom left is bit 56, and bottom right is bit 63, MSB.\n"
        "0 = Who plays next, in this case black\n"
        "0 = How many turns have been played.\n"
        "[] = this internally and later when the game is finished contains all the moves made as a list of\n"
        " {Who, X, Y, Board}-tuples. To make the protocol lighter, this is normally just an empty list.\n"
        "{2,2} = points by each player\n"
        "undefined, start time of the game (to be implemented)\n"
        "undefined, stop time of the game (to be implemented)\n"
        "{{0,300,0},{0,300,0}} = time left for each player in {megaseconds, seconds, microseconds} (to be implemented)\n"
        "\n"
        "--- Playing the game ---\n"
        "Once it is your turn, you may make your move, this is done by sending the server:\n"
        "{move, Cookie, Color, X, Y}\n"
        "where YourColor is either 0 (black) or 1 (white), and X and Y are integers between 0 and 7.\n"
        "\n"
        "Upon success you will either of the following replies:\n"
        "{ok, {please_wait, Game}}      -> Wait for the other bot to complete his move.\n"
        "{game_over, GameState, Winner} -> The game is over.\n"
        "\n"
        "After the please_wait message has been received, you will receive either of two messages:\n"
        "{ok, {your_move, Game}}        -> It is your turn.\n"
        "{game_over, GameState, Winner} -> The game is over, and the color Winner has won.\n"
        "\n"
        "--- Ending the game ---\n"
        "After finishing the game, close it by sending\n"
        "{logout}\n"
        "\n"
        "To this the server should respond with\n"
        "good_bye\n"
        "\n"
        "--- Other commands ---\n"
        "The following commands may be sent to the server when inside a game irrespective of the current protocol state:\n"
        "{available_moves, Game, Color} -> fetch the available moves for Color in Game.\n"
        "{game_status} -> fetch the status of the bots current game.\n"
        "{board} -> fetches a string representation of the board of the bots current game.\n"
        "{opponent} -> fetch the name of the bots current opponent.\n"
        "\n"
        "The following command may always be sent to the server:\n"
        "{bot_rank, BotName} -> fetch the current ranking of the bot called BotName\n".
