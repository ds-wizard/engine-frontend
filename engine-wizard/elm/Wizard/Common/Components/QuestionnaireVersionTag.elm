module Wizard.Common.Components.QuestionnaireVersionTag exposing
    ( current
    , version
    )

import Gettext exposing (gettext)
import Html exposing (Html, text)
import Shared.Components.Badge as Badge
import Shared.Data.QuestionnaireVersion exposing (QuestionnaireVersion)
import Wizard.Common.AppState exposing (AppState)


version : QuestionnaireVersion -> Html msg
version qv =
    Badge.secondary [] [ text qv.name ]


current : AppState -> Html msg
current appState =
    Badge.info [] [ text (gettext "Current" appState.locale) ]
