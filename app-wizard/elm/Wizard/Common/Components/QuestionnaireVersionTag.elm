module Wizard.Common.Components.QuestionnaireVersionTag exposing
    ( current
    , version
    )

import Gettext exposing (gettext)
import Html exposing (Html, text)
import Shared.Components.Badge as Badge
import Wizard.Common.AppState exposing (AppState)


version : { a | name : String } -> Html msg
version qv =
    Badge.secondary [] [ text qv.name ]


current : AppState -> Html msg
current appState =
    Badge.info [] [ text (gettext "Current" appState.locale) ]
