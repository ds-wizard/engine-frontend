module Compose exposing (compose2, compose3)


compose2 : (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 g f x y =
    g (f x y)


compose3 : (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
compose3 g f x y z =
    g (f x y z)
