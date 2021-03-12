module Wizard.Common.Components.QuestionnaireVersionTag exposing
    ( current
    , version
    )

import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Shared.Data.QuestionnaireVersion exposing (QuestionnaireVersion)
import Shared.Locale exposing (lx)
import Wizard.Common.AppState exposing (AppState)


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Common.Components.QuestionnaireVersionTag"


version : QuestionnaireVersion -> Html msg
version qv =
    span [ class "badge badge-secondary" ] [ text qv.name ]


current : AppState -> Html msg
current appState =
    span [ class "badge badge-info" ] [ lx_ "current" appState ]
