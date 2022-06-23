module Wizard.Common.Components.QuestionnaireVersionTag exposing
    ( current
    , version
    )

import Html exposing (Html, text)
import Shared.Components.Badge as Badge
import Shared.Data.QuestionnaireVersion exposing (QuestionnaireVersion)
import Shared.Locale exposing (lx)
import Wizard.Common.AppState exposing (AppState)


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Common.Components.QuestionnaireVersionTag"


version : QuestionnaireVersion -> Html msg
version qv =
    Badge.secondary [] [ text qv.name ]


current : AppState -> Html msg
current appState =
    Badge.info [] [ lx_ "current" appState ]
