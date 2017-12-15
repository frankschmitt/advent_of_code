# app
Haskell to the rescue (since I could not stand Clojure's abysmal runtime error messages + stacktraces)

## Installation
We use Haskell with HUnit for the tests.

## Usage
Running the tests (note the \_ in the filename!):
```
runhaskell -cpp \_StreamProcessing.hs
```

Solving the problem:
```
runhaskell -cpp -DSTANDALONE StreamProcessing.hs
```
