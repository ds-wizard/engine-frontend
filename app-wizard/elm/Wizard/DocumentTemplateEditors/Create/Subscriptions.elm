module Wizard.DocumentTemplateEditors.Create.Subscriptions exposing (subscriptions)

import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.DocumentTemplateEditors.Create.Models exposing (Model)
import Wizard.DocumentTemplateEditors.Create.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map DocumentTemplateTypeHintInputMsg <|
        TypeHintInput.subscriptions model.documentTemplateTypeHintInputModel
