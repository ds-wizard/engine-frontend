module Public.Questionnaire.View exposing (view)

import Common.AppState exposing (AppState)
import Common.Html exposing (linkTo)
import Common.Locale exposing (l, lh, lx)
import Common.Questionnaire.DefaultQuestionnaireRenderer exposing (defaultQuestionnaireRenderer)
import Common.Questionnaire.Models.QuestionnaireFeature as QuestionnaireFeature
import Common.Questionnaire.View exposing (viewQuestionnaire)
import Common.View.Page as Page
import Html exposing (..)
import Html.Attributes exposing (class)
import Public.Questionnaire.Models exposing (Model)
import Public.Questionnaire.Msgs exposing (Msg(..))
import Routing exposing (signupRoute)


l_ : String -> AppState -> String
l_ =
    l "Public.Questionnaire.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Public.Questionnaire.View"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Public.Questionnaire.View"


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
