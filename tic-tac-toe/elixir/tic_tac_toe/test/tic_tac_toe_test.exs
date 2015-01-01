defmodule TicTacToeTest do
  use ExUnit.Case

  def testy(sym, {sym}) do
    true
  end

  test "testy" do
    # assert testy([:x], :x) == true
    assert testy(:x, {:x}) == true
  end

  # test "the truth" do
  #   assert TicTacToe.check_winner([:x, :z, :z, :z, :z, :z, :z, :z, :z ], :x) == false
  # end
end
