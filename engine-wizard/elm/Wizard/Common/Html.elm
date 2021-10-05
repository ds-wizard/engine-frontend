module Wizard.Common.Html exposing
    ( illustratedMessage
    , linkTo
    , resizableTextarea
    )

import Html exposing (Attribute, Html, a, div, img, p, text, textarea)
import Html.Attributes exposing (class, rows, src, value)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (grammarlyAttributes, linkToAttributes)
import Wizard.Routes as Routes


linkTo : AppState -> Routes.Route -> List (Attribute msg) -> List (Html msg) -> Html msg
linkTo appState route attributes children =
    a (attributes ++ linkToAttributes appState route) children


illustratedMessage : String -> String -> Html msg
illustratedMessage image message =
    div [ class "illustrated-message" ]
        [ img [ src <| "/img/illustrations/undraw_" ++ image ++ ".svg" ] []
        , p [] [ text message ]
        ]


resizableTextarea : Int -> String -> List (Attribute msg) -> List (Html msg) -> Html msg
resizableTextarea minRows editValue attributes =
    let
        textAreaRows =
            String.split "\n" editValue
                |> List.length
                |> max minRows
    in
    textarea ([ value editValue, rows textAreaRows ] ++ grammarlyAttributes ++ attributes)
