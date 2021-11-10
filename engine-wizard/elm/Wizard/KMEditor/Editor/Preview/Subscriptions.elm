module Wizard.KMEditor.Editor.Preview.Subscriptions exposing (subscriptions)

import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.KMEditor.Editor.Preview.Models exposing (Model)
import Wizard.KMEditor.Editor.Preview.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map QuestionnaireMsg <|
        Questionnaire.subscriptions model.questionnaireModel
