defmodule TicTacToe do
  require Logger
  use GenServer

  def start_link(), do: GenServer.start_link(__MODULE__, [], [])

  # Make a move
  def move(pid, player, x, y), do: GenServer.call(pid, {player, x, y})

  # Prints game field
  def print_game(pid), do: GenServer.call(pid, :print)

  # Init empty game field
  def init([]) do
    {:ok, {[:z, :z, :z, :z, :z, :z, :z, :z, :z], :nobody}}
  end

  def terminate(_status, _state), do: :ok

  # The same player attempts to move twice
  def handle_call({last_player, _x, _y}, _from, {game, last_player}) do
    {:reply, "Wrong player!", {game, last_player}}
  end

  # Regular move
  def handle_call({player, x, y}, _from, {game, last_player}) when player == :o or player == :x do
    case check_pos(game, x, y) do
      :z ->
        new_game = set_player(game, player, x, y)
        case check_winner(player, new_game) do
          true  -> {:stop, :normal, "Winner!", {new_game, player}}
          false -> {:reply, "Next", {new_game, player}}
        end
      _ ->
        {:reply, "Wrong move!", {game, last_player}}
    end
  end

  # Print game field
  def handle_call(:print, _from, {game, last_player}) do
    reply = :io_lib.format("~p~n~p~n~p~n~nLast Player: ~p~n",
                            [:lists.sublist(game, 3),
                             :lists.sublist(game, 4, 3),
                             :lists.sublist(game, 7, 3), last_player])
    {:reply, reply, {game, last_player}}
  end

  # Sets a player to a position
  defp set_player(game, player, x, y) do
    pos = calc_position(x, y)
    :lists.sublist(game, pos) ++ [player] ++ :lists.nthtail(pos + 1, game)
  end

  # Calculate position in list
  defp calc_position(x, y), do: x + y * 3

  # Checks if position is available
  defp check_pos(game, x, y), do: :lists.nth(calc_position(x, y) + 1, game)

  # Checks if there's a winner
  # Horizontals
  defp check_winner(p, [p, p, p, _, _, _, _, _, _ ]), do: true
  defp check_winner(p, [_, _, _, p, p, p, _, _, _ ]), do: true
  defp check_winner(p, [_, _, _, _, _, _, p, p, p ]), do: true

  # Verticals
  defp check_winner(p, [p, _, _, p, _, _, p, _, _ ]), do: true
  defp check_winner(p, [_, p, _, _, p, _, _, p, _ ]), do: true
  defp check_winner(p, [_, _, p, _, _, p, _, _, p ]), do: true

  # Diagonals
  defp check_winner(p, [p, _, _, _, p, _, _, _, p ]), do: true
  defp check_winner(p, [_, _, p, _, p, _, p, _, _ ]), do: true

  defp check_winner(_, [_, _, _, _, _, _, _, _, _ ]), do: false

end
