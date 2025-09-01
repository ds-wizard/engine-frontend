module Wizard.Pages.Projects.Migration.Subscriptions exposing (subscriptions)

import Wizard.Components.Questionnaire as Questionnaire
import Wizard.Pages.Projects.Migration.Models exposing (Model)
import Wizard.Pages.Projects.Migration.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.questionnaireModel of
        Just questionnaireModel ->
            Sub.map QuestionnaireMsg <|
                Questionnaire.subscriptions questionnaireModel

        _ ->
            Sub.none
