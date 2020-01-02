module Wizard.Public.Questionnaire.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class)
import Shared.Locale exposing (l, lh, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Questionnaire.DefaultQuestionnaireRenderer exposing (defaultQuestionnaireRenderer)
import Wizard.Common.Questionnaire.Models.QuestionnaireFeature as QuestionnaireFeature
import Wizard.Common.Questionnaire.View exposing (viewQuestionnaire)
import Wizard.Common.View.Page as Page
import Wizard.Public.Questionnaire.Models exposing (Model)
import Wizard.Public.Questionnaire.Msgs exposing (Msg(..))
import Wizard.Routing as Routing exposing (signupRoute)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Public.Questionnaire.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Public.Questionnaire.View"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.Public.Questionnaire.View"


view : AppState -> Model -> Html Msg
view appState model =
    div [ class "Public__Questionnaire" ]
        [ info appState
        , Page.actionResultView appState
            (viewQuestionnaire
                { features = [ QuestionnaireFeature.feedback ]
                , levels = Nothing
                , getExtraQuestionClass = always Nothing
                , forceDisabled = False
                , createRenderer = defaultQuestionnaireRenderer appState
                }
                appState
                >> Html.map QuestionnaireMsg
            )
            model.questionnaireModel
        ]


info : AppState -> Html Msg
info appState =
    div [ class "alert alert-info" ]
        [ h4 [ class "alert-heading" ] [ lx_ "title" appState ]
        , p []
            (lh_ "text"
                [ linkTo appState signupRoute [ class "alert-link" ] [ lx_ "signUp" appState ]
                ]
                appState
            )
        ]
