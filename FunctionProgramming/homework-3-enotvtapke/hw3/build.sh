stack build
stack test

stylish-haskell -c stylish-haskell.yaml -i ./src/HW3/Base.hs
stylish-haskell -c stylish-haskell.yaml -i ./src/HW3/Parser.hs
stylish-haskell -c stylish-haskell.yaml -i ./src/HW3/Pretty.hs
stylish-haskell -c stylish-haskell.yaml -i ./src/HW3/Evaluator.hs
stylish-haskell -c stylish-haskell.yaml -i ./src/HW3/Action.hs

hlint ./src -h hlint.yaml