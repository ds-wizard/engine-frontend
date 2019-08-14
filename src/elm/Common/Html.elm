module Common.Html exposing
    ( emptyNode
    , fa
    , faSet
    , linkTo
    )

import Common.AppState exposing (AppState)
import Common.Html.Attribute exposing (linkToAttributes)
import Common.Provisioning.DefaultIconSet as DefaultIconSet
import Dict
import Html exposing (..)
import Html.Attributes exposing (class)
import Maybe.Extra as Maybe
import Routes


linkTo : AppState -> Routes.Route -> List (Attribute msg) -> List (Html msg) -> Html msg
linkTo appState route attributes children =
    a (attributes ++ linkToAttributes appState route) children


emptyNode : Html msg
emptyNode =
    text ""


fa : String -> Html msg
fa icon =
    i [ class <| "fa fa-" ++ icon ] []


faSet : String -> AppState -> Html msg
faSet iconKey appState =
    Dict.get iconKey appState.provisioning.iconSet
        |> Maybe.orElseLazy (\_ -> Dict.get iconKey DefaultIconSet.iconSet)
        |> Maybe.map fa
        |> Maybe.withDefault emptyNode
