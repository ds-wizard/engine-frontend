module Wizard.Pages.DocumentTemplateEditors.Create.Subscriptions exposing (subscriptions)

import Wizard.Components.TypeHintInput as TypeHintInput
import Wizard.Pages.DocumentTemplateEditors.Create.Models exposing (Model)
import Wizard.Pages.DocumentTemplateEditors.Create.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map DocumentTemplateTypeHintInputMsg <|
        TypeHintInput.subscriptions model.documentTemplateTypeHintInputModel
