module Shared.Html exposing
    ( emptyNode
    , fa
    , faKey
    , faKeyClass
    , faSet
    )

import Dict
import Html exposing (Html, i, text)
import Html.Attributes exposing (class)
import Shared.Provisioning exposing (Provisioning)


emptyNode : Html msg
emptyNode =
    text ""



-- Font Awesome


fa : String -> Html msg
fa icon =
    i [ class <| "fa " ++ icon ] []


faSet : String -> { a | provisioning : Provisioning } -> Html msg
faSet iconKey appState =
    faKey iconKey appState
        |> Maybe.map fa
        |> Maybe.withDefault emptyNode


faKeyClass : String -> { a | provisioning : Provisioning } -> String
faKeyClass iconKey appState =
    case faKey iconKey appState of
        Just key ->
            "fa " ++ key

        Nothing ->
            ""


faKey : String -> { a | provisioning : Provisioning } -> Maybe String
faKey iconKey appState =
    Dict.get iconKey appState.provisioning.iconSet
