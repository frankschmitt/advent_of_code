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

  @doc """ 
    Solve part I of the puzzle
  """
  def step_n(state, _width, 0) do
    state
  end

  def step_n(state, width, n) do
    step_n(step(state, width), width, n-1)
  end

  @doc """ 
  Initialize the circular buffer for the spinlock, part II.

  ## Examples

      iex> Spinlock.init2 
      %{position: 0, last_value: 0, value_after_0: -1  } 

  """
  def init2 do
    %{ position: 0, last_value: 0, value_after_0: -1 }
  end

  @doc """
  Perform the next state in the spinlock algorithm.

  ## Examples

      iex> Spinlock.step2(Spinlock.init2, 3)
      %{ position: 1, last_value: 1, value_after_0: 1 }

  """
  def step2(state, width) do
    # last_value + 1 == length(buffer)
    new_position = rem(state.position + width, state.last_value + 1) + 1
    new_value = state.last_value + 1
    new_value_after_0 = 
      case new_position do
        1 -> IO.puts("new value at pos 1: #{new_value}"); new_value
        _ -> state.value_after_0
      end
    %{ position: new_position, last_value: new_value, value_after_0: new_value_after_0 } 
  end


  @doc """ 
    Solve part II of the puzzle
  """
  def step_n2(state, _width, 0) do
    state
  end

  def step_n2(state, width, n) do
    step_n2(step2(state, width), width, n-1)
  end
end
