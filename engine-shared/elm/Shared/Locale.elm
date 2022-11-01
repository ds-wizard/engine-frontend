module Shared.Locale exposing (lr)

import Dict
import Shared.Provisioning exposing (Provisioning)


l : String -> String -> { a | provisioning : Provisioning } -> String
l moduleKey termKey appState =
    let
        key =
            moduleKey ++ "." ++ termKey
    in
    Dict.get key appState.provisioning.locale
        |> Maybe.withDefault ""


lr : String -> { a | provisioning : Provisioning } -> String
lr =
    l "__routing"
