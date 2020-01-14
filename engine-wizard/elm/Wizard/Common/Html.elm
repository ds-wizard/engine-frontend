module Wizard.Common.Html exposing
    ( emptyNode
    , fa
    , faKeyClass
    , faSet
    , linkTo
    )

import Dict
import Html exposing (..)
import Html.Attributes exposing (class)
import Maybe.Extra as Maybe
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (linkToAttributes)
import Wizard.Common.Provisioning.DefaultIconSet as DefaultIconSet
import Wizard.Routes as Routes


linkTo : AppState -> Routes.Route -> List (Attribute msg) -> List (Html msg) -> Html msg
linkTo appState route attributes children =
    a (attributes ++ linkToAttributes appState route) children


emptyNode : Html msg
emptyNode =
    text ""


fa : String -> Html msg
fa icon =
    i [ class <| "fa " ++ icon ] []


faSet : String -> AppState -> Html msg
faSet iconKey appState =
    faKey iconKey appState
        |> Maybe.map fa
        |> Maybe.withDefault emptyNode


faKey : String -> AppState -> Maybe String
faKey iconKey appState =
    Dict.get iconKey appState.provisioning.iconSet
        |> Maybe.orElseLazy (\_ -> Dict.get iconKey DefaultIconSet.iconSet)


faKeyClass : String -> AppState -> String
faKeyClass iconKey appState =
    case faKey iconKey appState of
        Just key ->
            "fa " ++ key

        Nothing ->
            ""
