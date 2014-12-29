-module(game).
-export([start/0, print_game/1]).


%% Entry point
start() -> loop([z, z, z, z, z, z, z, z, z]).

%% Sets a player to a position
set_player(Game, Player, X, Y) ->
  Pos = X + Y * 3,
  lists:sublist(Game, Pos) ++ [Player] ++ lists:nthtail(Pos + 1, Game).

%% Checks if position is available
check_pos(Game, X, Y) -> lists:nth(X + Y * 3 + 1, Game).

%% Checks if there's a winner
check_winner(Game) ->
  check_winner(Game, 0) or check_winner(Game, x).

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

%% Prints game field
print_game(Game) ->
  io:format("~p~n~p~n~p~n~n", [lists:sublist(Game, 3), lists:sublist(Game, 4, 3), lists:sublist(Game, 7, 3)]).

%% Returns next player
next_player(LastPlayer) ->
  case LastPlayer of
    0 -> x;
    x -> 0
  end.

%% Main loop
loop(Game) ->
  io:format("Starting a new game.~n"),
  loop(Game, 0).

loop(Game, LastPlayer) ->
  print_game(Game),
  Player = next_player(LastPlayer),
  receive
    {Player, X, Y} ->
      case check_pos(Game, X, Y) of
        z ->
          NewGame = set_player(Game, Player, X, Y),
          case check_winner(NewGame) of
            false -> loop(NewGame, Player);
            _true -> io:format("Winner!~n")
          end;
        _ ->
          io:format("Wrong move!~n"),
          loop(Game, Player)
      end;
    {_Player, _X, _Y} ->
      io:format("Wrong player!~n"),
      loop(Game, LastPlayer)
  end.
