module Public.Questionnaire.View exposing (..)

import Common.Html exposing (linkTo)
import Common.Questionnaire.View exposing (viewQuestionnaire)
import Common.View exposing (fullPageActionResultView)
import Html exposing (..)
import Html.Attributes exposing (class)
import Msgs
import Public.Questionnaire.Models exposing (Model)
import Public.Questionnaire.Msgs exposing (Msg(..))
import Routing exposing (signupRoute)


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    div [ class "Public__Questionnaire" ]
        [ info
        , fullPageActionResultView (viewQuestionnaire { showExtraActions = False } >> Html.map (QuestionnaireMsg >> wrapMsg)) model.questionnaireModel
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
