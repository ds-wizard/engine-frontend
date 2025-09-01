module Wizard.Projects.Migration.Subscriptions exposing (subscriptions)

import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Projects.Migration.Models exposing (Model)
import Wizard.Projects.Migration.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.questionnaireModel of
        Just questionnaireModel ->
            Sub.map QuestionnaireMsg <|
                Questionnaire.subscriptions questionnaireModel

        _ ->
            Sub.none
