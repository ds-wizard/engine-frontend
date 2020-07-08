module WizardResearch.Page exposing
    ( Page(..)
    , view
    )

import Browser exposing (Document)
import Css exposing (Style, paddingTop)
import Html.Styled exposing (Html, div, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Shared.Data.BootstrapConfig
import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Shared.Elemental.Components.Navigation as Navigation
import Shared.Elemental.Foundations.Size as Size
import Shared.Elemental.Global as Global
import Shared.Elemental.Theme exposing (Theme)
import Shared.Elemental.Utils exposing (px2rem)
import WizardResearch.Common.AppState as AppState exposing (AppState)


type Page
    = App
    | Auto
    | Public


view : AppState -> Page -> { title : String, content : Html msg } -> Document msg
view appState page { title, content } =
    { title = title ++ " | " ++ LookAndFeelConfig.getAppTitle appState.config.lookAndFeel
    , body = [ toUnstyled <| layout appState page content ]
    }


layout : AppState -> Page -> Html msg -> Html msg
layout appState page =
    case page of
        App ->
            layoutApp appState

        Auto ->
            layoutAuto appState

        Public ->
            layoutPublic appState


layoutStyle : Theme -> List Style
layoutStyle theme =
    [ Global.styles theme
    ]


layoutApp : AppState -> Html msg -> Html msg
layoutApp appState content =
    div [ css (paddingTop (px2rem Size.navigationHeight) :: layoutStyle appState.theme) ]
        [ Navigation.view
            { appTitle = LookAndFeelConfig.getAppTitle appState.config.lookAndFeel
            , theme = appState.theme
            }
        , content
        ]


layoutAuto : AppState -> Html msg -> Html msg
layoutAuto appState =
    if AppState.authenticated appState then
        layoutApp appState

    else
        layoutPublic appState


layoutPublic : AppState -> Html msg -> Html msg
layoutPublic appState content =
    div [ css (layoutStyle appState.theme) ]
        [ content
        ]
