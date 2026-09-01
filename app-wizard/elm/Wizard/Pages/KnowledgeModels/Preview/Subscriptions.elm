module Wizard.Pages.KnowledgeModels.Preview.Subscriptions exposing (subscriptions)

import ActionResult exposing (ActionResult(..))
import Wizard.Components.Questionnaire as Questionnaire
import Wizard.Pages.KnowledgeModels.Preview.Models exposing (Model)
import Wizard.Pages.KnowledgeModels.Preview.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.questionnaireModel of
        Success questionnaireModel ->
            Sub.map QuestionnaireMsg <|
                Questionnaire.subscriptions questionnaireModel

        _ ->
            Sub.none
