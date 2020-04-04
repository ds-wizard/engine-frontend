module Shared.Setters exposing (..)


setToken : a -> { b | token : a } -> { b | token : a }
setToken value record =
    { record | token = value }
