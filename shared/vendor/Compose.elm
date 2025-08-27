module Compose exposing (compose2)


compose2 : (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 g f x y =
    g (f x y)
