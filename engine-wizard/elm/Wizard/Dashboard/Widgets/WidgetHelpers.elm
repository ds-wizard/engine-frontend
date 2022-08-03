module Wizard.Dashboard.Widgets.WidgetHelpers exposing
    ( CtaWidgetConfig
    , ctaWidget
    , widget
    , widgetError
    , widgetLoader
    )

import Html exposing (Html, div, h2, span, text)
import Html.Attributes exposing (class)
import Shared.Html exposing (faSet)
import Shared.Markdown as Markdown
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Routes as Routes


widget : List (Html msg) -> Html msg
widget elements =
    div [ class "col-12 col-lg-6 mb-3" ]
        [ div [ class "p-4 bg-light rounded-3 h-100" ] elements ]


type alias CtaWidgetConfig =
    { title : String
    , text : String
    , action :
        { route : Routes.Route
        , label : String
        }
    }


ctaWidget : AppState -> CtaWidgetConfig -> Html msg
ctaWidget appState cfg =
    widget
        [ div [ class "d-flex flex-column h-100" ]
            [ h2 [ class "fs-4 fw-bold mb-4" ] [ text cfg.title ]
            , Markdown.toHtml [ class "mb-4 flex-grow-1" ] cfg.text
            , div []
                [ linkTo appState
                    cfg.action.route
                    [ class "btn btn-primary btn-wide" ]
                    [ text cfg.action.label ]
                ]
            ]
        ]


widgetLoader : AppState -> Html msg
widgetLoader appState =
    div [ class "h-100 fs-1 d-flex justify-content-center align-items-center text-lighter animation-fade-in" ]
        [ faSet "_global.spinner" appState
        ]


widgetError : AppState -> String -> Html msg
widgetError appState errorText =
    div [ class "h-100 fs-5 d-flex justify-content-center align-items-center text-lighter" ]
        [ span [ class "me-2" ] [ faSet "_global.warning" appState ]
        , text errorText
        ]
