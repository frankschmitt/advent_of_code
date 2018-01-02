# Synopsis
Solution for the knot hash challenge, written in [Clojure|https://clojure.org/ ]

# Usage

## Installation
Install Clojure according to the instructions on the Clojure homepage.

## Running the tests
```
lein test
```

or (from the REPL)
```
(require '[clojure.test :refer [run-tests]])
(require 'knot-hash.core-test)
(run-tests 'knot-hash.core-test)
```

To reload changed code in the repl, use
```
(use 'knot-hash.core-test :reload)
(use 'knot-hash.core :reload)
(run-tests 'knot-hash.core-test)
```

## License

Copyright © 2017 Frank Schmitt

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
