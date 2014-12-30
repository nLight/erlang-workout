-module(game).
-behaviour(gen_server).
-export([init/1, handle_call/3, terminate/2]).
-export([start_link/0, move/4, print_game/1]).

start_link() -> gen_server:start_link(?MODULE, [], []).

%% Make a move
move(Pid, Player, X, Y) ->
  gen_server:call(Pid, {Player, X, Y}).

%% Prints game field
print_game(Pid) -> gen_server:call(Pid, print).

%% Server
init([]) ->
  {ok, {[z, z, z, z, z, z, z, z, z], nobody}}.

terminate(_Status, _State) -> ok.

handle_call({LastPlayer, _X, _Y}, _From, {Game, LastPlayer}) ->
  {reply, "Wrong player!", {Game, LastPlayer}};

handle_call({Player, X, Y}, _From, {Game, _LastPlayer}) when Player =:= o ; Player =:= x ->
  case check_pos(Game, X, Y) of
    z ->
      NewGame = set_player(Game, Player, X, Y),
      case check_winner(NewGame, Player) of
        true  -> {stop, normal, "Winner!", {NewGame, Player}};
        false -> {reply, "Next", {NewGame, Player}}
      end;
    _ ->
      {reply, "Wrong move!", {Game, Player}}
  end;

handle_call(print, _From, {Game, LastPlayer}) ->
  Reply = io_lib:format("~p~n~p~n~p~n~nLast Player: ~p~n",
                        [lists:sublist(Game, 3),
                         lists:sublist(Game, 4, 3),
                         lists:sublist(Game, 7, 3), LastPlayer]),
  {reply, Reply, {Game, LastPlayer}}.

%% Sets a player to a position
set_player(Game, Player, X, Y) ->
  Pos = X + Y * 3,
  lists:sublist(Game, Pos) ++ [Player] ++ lists:nthtail(Pos + 1, Game).

%% Checks if position is available
check_pos(Game, X, Y) -> lists:nth(X + Y * 3 + 1, Game).

%% Checks if there's a winner
check_winner(Game, P) ->
  case Game of
    %% Horizontals
    [P, P, P, _, _, _, _, _, _ ] -> true;
    [_, _, _, P, P, P, _, _, _ ] -> true;
    [_, _, _, _, _, _, P, P, P ] -> true;
    %% Verticals
    [P, _, _, P, _, _, P, _, _ ] -> true;
    [_, P, _, _, P, _, _, P, _ ] -> true;
    [_, _, P, _, _, P, _, _, P ] -> true;
    %% Diagonals
    [P, _, _, _, P, _, _, _, P ] -> true;
    [_, _, P, _, P, _, P, _, _ ] -> true;
    _else -> false
  end.
