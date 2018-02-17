# Synopsis
Solution for the spinlock puzzle, written in Elixir.

# Overall approach
## Part I
To store the current state of our spinlock simulation, we use a record containing:
- the buffer (as a list)
- the position
- the last value that was inserted

Solving Part I is then business as usual:
- initialize the record
- compute the next step (next position is current position plus width modulo length plus one (since we want to insert after instead of before)
- perform the step N times
- search for 2017 in our result buffer, and report the value directly after it 

## Part II
Although storing a list with 50M records might be feasible, the constant re-allocation / shuffling prevents us from using the simple simulation we used for part I. But we know that 0 is always the first element in the list (it never gets moved). Exploiting this fact, we don't need to store the entire list - we only have to store
- the current length of the "virtual" list
- the last value that was inserted
- the last element that was inserted at position 1 (directly after 0) 

With that modified approach, solving Part II is totally analogous to Part I:
- initialize the record
- compute the next step
- perform the step N times (50M in our case)
- report the value directly after 0 


# Usage

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `spinlock` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [{:spinlock, "~> 0.1.0"}]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/spinlock](https://hexdocs.pm/spinlock).

## REPL
```iex -S mix```

## Running the tests
```mix test```

## Solving the puzzle
Just run the tests.
