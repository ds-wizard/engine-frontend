module Public.Questionnaire.View exposing (view)

import Common.AppState exposing (AppState)
import Common.Html exposing (linkTo)
import Common.Questionnaire.DefaultQuestionnaireRenderer exposing (defaultQuestionnaireRenderer)
import Common.Questionnaire.View exposing (viewQuestionnaire)
import Common.View.Page as Page
import Html exposing (..)
import Html.Attributes exposing (class)
import Msgs
import Public.Questionnaire.Models exposing (Model)
import Public.Questionnaire.Msgs exposing (Msg(..))
import Routing exposing (signupRoute)


view : (Msg -> Msgs.Msg) -> AppState -> Model -> Html Msgs.Msg
view wrapMsg appState model =
    div [ class "Public__Questionnaire" ]
        [ info
        , Page.actionResultView
            (viewQuestionnaire
                { showExtraActions = appState.config.feedbackEnabled
                , showExtraNavigation = False
                , levels = Nothing
                , getExtraQuestionClass = always Nothing
                , forceDisabled = False
                , createRenderer = defaultQuestionnaireRenderer
                }
                appState
                >> Html.map (QuestionnaireMsg >> wrapMsg)
            )
            model.questionnaireModel
        ]


info : Html Msgs.Msg
info =
    div [ class "alert alert-info" ]
        [ h4 [ class "alert-heading" ] [ text "Questionnaire demo" ]
        , p []
            [ text "You can browse questions and answers in this questionnaire demo. If you want to save the results or try more functionality (e.g. generating data management plans), you need to "
            , linkTo signupRoute [ class "alert-link" ] [ text "sign up" ]
            , text " first."
            ]
        ]
