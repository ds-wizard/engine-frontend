module Shared.Locale exposing
    ( l
    , lf
    , lg
    , lgf
    , lgh
    , lgx
    , lh
    , lr
    , lx
    )

import Dict
import Html exposing (Html, text)
import Shared.Provisioning exposing (Provisioning)
import String.Format as String


l : String -> String -> { a | provisioning : Provisioning } -> String
l moduleKey termKey appState =
    let
        key =
            moduleKey ++ "." ++ termKey
    in
    Dict.get key appState.provisioning.locale
        |> Maybe.withDefault ""


lx : String -> String -> { a | provisioning : Provisioning } -> Html msg
lx m t =
    text << l m t


lg : String -> { a | provisioning : Provisioning } -> String
lg =
    l "_global"


lr : String -> { a | provisioning : Provisioning } -> String
lr =
    l "__routing"


lgx : String -> { a | provisioning : Provisioning } -> Html msg
lgx t =
    text << lg t


lgf : String -> List String -> { a | provisioning : Provisioning } -> String
lgf =
    lf "_global"


lf : String -> String -> List String -> { a | provisioning : Provisioning } -> String
lf moduleKey termKey terms appState =
    String.format (l moduleKey termKey appState) terms


lh : String -> String -> List (Html msg) -> { a | provisioning : Provisioning } -> List (Html msg)
lh moduleKey termKey elements appState =
    String.formatHtml (l moduleKey termKey appState) elements


lgh : String -> List (Html msg) -> { a | provisioning : Provisioning } -> List (Html msg)
lgh =
    lh "_global"
