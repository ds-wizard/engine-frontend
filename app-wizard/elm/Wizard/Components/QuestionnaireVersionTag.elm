module Wizard.Components.QuestionnaireVersionTag exposing
    ( current
    , version
    )

import Common.Components.Badge as Badge
import Gettext exposing (gettext)
import Html exposing (Html, text)
import Wizard.Data.AppState exposing (AppState)


version : { a | name : String } -> Html msg
version qv =
    Badge.secondary [] [ text qv.name ]


current : AppState -> Html msg
current appState =
    Badge.info [] [ text (gettext "Current" appState.locale) ]
