module Wizard.KnowledgeModels.Preview.Subscriptions exposing (subscriptions)

import ActionResult exposing (ActionResult(..))
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.KnowledgeModels.Preview.Models exposing (Model)
import Wizard.KnowledgeModels.Preview.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.questionnaireModel of
        Success questionnaireModel ->
            Sub.map QuestionnaireMsg <|
                Questionnaire.subscriptions questionnaireModel

        _ ->
            Sub.none
