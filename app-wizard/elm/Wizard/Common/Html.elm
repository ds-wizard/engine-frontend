module Wizard.Common.Html exposing
    ( guideLink
    , illustratedMessage
    , linkTo
    , resizableTextarea
    )

import Gettext exposing (gettext)
import Html exposing (Attribute, Html, a, div, p, text, textarea)
import Html.Attributes exposing (class, href, rows, target, value)
import Shared.Components.FontAwesome exposing (faGuideLink)
import Wizard.Common.AppState as AppState exposing (AppState)
import Wizard.Common.GuideLinks as GuideLinks exposing (GuideLinks)
import Wizard.Common.Html.Attribute exposing (grammarlyAttributes, linkToAttributes, tooltipLeft)
import Wizard.Routes as Routes


linkTo : Routes.Route -> List (Attribute msg) -> List (Html msg) -> Html msg
linkTo route attributes children =
    a (attributes ++ linkToAttributes route) children


guideLink : AppState -> (GuideLinks -> String) -> Html msg
guideLink appState getLink =
    a
        (href (GuideLinks.wrap (AppState.toServerInfo appState) (getLink appState.guideLinks))
            :: class "guide-link"
            :: target "_blank"
            :: tooltipLeft (gettext "Learn more in guide" appState.locale)
        )
        [ faGuideLink ]


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
    textarea ([ value editValue, rows textAreaRows ] ++ grammarlyAttributes ++ attributes)
