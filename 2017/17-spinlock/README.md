# Synopsis
Solution for the spinlock puzzle, written in Elixir.

# Overall approach
To store the current state of our spinlock simulation, we use a record containing:
- the buffer (as a list)
- the position
- the last value that was inserted

Part I is then business as usual:
- initialize the record
- compute the next step (next position is current position plus width modulo length plus one (since we want to insert after instead of before)
- perform the step N times
- search for 2017 in our result buffer, and report the value directly after it 

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

