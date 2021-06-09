module Wizard.Projects.Create.TemplateCreate.Subscriptions exposing (..)

import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Projects.Create.TemplateCreate.Models exposing (Model)
import Wizard.Projects.Create.TemplateCreate.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map QuestionnaireTypeHintInputMsg <|
        TypeHintInput.subscriptions model.questionnaireTypeHintInputModel
