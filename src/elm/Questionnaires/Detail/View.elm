module Questionnaires.Detail.View exposing (..)

import Common.View exposing (fullPageActionResultView, pageHeader)
import Html exposing (..)
import Html.Attributes exposing (..)
import Msgs
import Questionnaires.Common.Models exposing (QuestionnaireDetail)
import Questionnaires.Detail.Models exposing (Model)
import Questionnaires.Detail.Msgs exposing (Msg)


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    fullPageActionResultView (content wrapMsg) model.questionnaire


content : (Msg -> Msgs.Msg) -> QuestionnaireDetail -> Html Msgs.Msg
content wrapMsg questionnaire =
    div [ class "questionnaire-detail" ]
        [ pageHeader (questionnaire.name ++ " (" ++ questionnaire.package.name ++ ", " ++ questionnaire.package.version ++ ")") []
        ]
