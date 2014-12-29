-module(game).
-export([start/0, print_game/1]).


%% Entry point
start() -> loop([z, z, z, z, z, z, z, z, z]).

%% Sets a player to a position
set_player(Game, Player, X, Y) ->
  Pos = X + Y * 3,
  lists:sublist(Game, Pos) ++ [Player] ++ lists:nthtail(Pos + 1, Game).

%% Prints game field
print_game(Game) ->
  io:format("~p~n~p~n~p~n~n", [lists:sublist(Game, 3), lists:sublist(Game, 4, 3), lists:sublist(Game, 7, 3)]).

%% Main loop
loop(Game) -> 
  io:format("Starting a new game.~n"),
  loop(Game, 0).


loop(Game, x) ->
  print_game(Game),
  receive
    {0, X, Y} ->
      NewGame = set_player(Game, 0, X, Y),
      loop(NewGame, 0);
    {_Player, _X, _Y} ->
      io:format("Wrong player!"),
      loop(Game, x)
  end;
loop(Game, 0) ->
  print_game(Game),
  receive
    {x, X, Y} ->
      NewGame = set_player(Game, x, X, Y),
      loop(NewGame, x);
    {_Player, _X, _Y} ->
      io:format("Wrong player!"),
      loop(Game, 0)
  end.

