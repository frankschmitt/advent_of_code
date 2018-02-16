defmodule Spinlock do
  @moduledoc """
  Documentation for Spinlock.
  """

  @doc """ 
  Initialize the circular buffer for the spinlock.

  ## Examples

      iex> Spinlock.init 
      %{buffer: [0], position: 0, last_value: 0 } 

  """
  def init do
    %{ buffer: [0], position: 0, last_value: 0 }
  end

  @doc """
  Perform the next state in the spinlock algorithm.

  ## Examples

      iex> Spinlock.step(Spinlock.init, 3)
      %{ buffer: [0,1], position: 1, last_value: 1 }

  """
  def step(state, width) do
    # last_value + 1 == length(buffer)
    new_position = rem(state.position + width, state.last_value + 1) + 1
    new_value = state.last_value + 1
    new_buffer = List.insert_at(state.buffer, new_position, new_value)
    %{ buffer: new_buffer, position: new_position, last_value: new_value } 
  end

  def step_n(state, _width, 0) do
    state
  end

  def step_n(state, width, n) do
    if (rem(n, 1000) == 0) do
      IO.puts(n)
    end
    step_n(step(state, width), width, n-1)
  end
end
