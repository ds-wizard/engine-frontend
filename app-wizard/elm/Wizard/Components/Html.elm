module Wizard.Components.Html exposing
    ( illustratedMessage
    , linkTo
    , resizableTextarea
    )

import Html exposing (Attribute, Html, a, div, p, text, textarea)
import Html.Attributes exposing (class, rows, value)
import Html.Attributes.Extensions exposing (disableGrammarly)
import Wizard.Routes as Routes
import Wizard.Utils.HtmlAttributesUtils exposing (linkToAttributes)


linkTo : Routes.Route -> List (Attribute msg) -> List (Html msg) -> Html msg
linkTo route attributes children =
    a (attributes ++ linkToAttributes route) children


illustratedMessage : Html msg -> String -> Html msg
illustratedMessage image message =
    div [ class "illustrated-message" ]
        [ image
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
    textarea ([ value editValue, rows textAreaRows, disableGrammarly ] ++ attributes)
