-module(cs_db).

%% API
-export([ create_table/0
        , add_user/2
        , get_password/1
        ]).

-record(user, {username, password}).

-define(TABLE, user).
-define(TABLE_ATTR, {attributes, record_info(fields,user)}).

create_table() ->
  Opts = [{type, set}, {disc_copies, [node()]}, {ram_copies, nodes()}],
  mnesia:create_table(?TABLE, [?TABLE_ATTR|Opts]).

add_user(Username, Password) ->
  F = fun() ->
          case read(Username) of
            no_such_user ->
              write(#user{username = Username, password = Password});
            _ ->
              {error, could_not_add_user}
          end
      end,
  case mnesia:transaction(F) of
    {atomic, ok} -> true;
    {atomic, {error, could_not_add_user}} -> false;
    _ -> false
  end.

get_password(Username) ->
  case read(Username) of
    #user{password = Password} ->
      {ok, Password};
    _ ->
      no_such_user
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions

write(#user{} = User) ->
  {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(User) end).

read(#user{username = Username}) ->
  read(Username);
read(Username) when is_list(Username)->
  case mnesia:transaction(fun() -> mnesia:read(?TABLE, Username) end) of
    {atomic, [User]} ->
      User;
    {atomic, []} ->
      no_such_user;
    Other ->
      {error, Other}
  end.
