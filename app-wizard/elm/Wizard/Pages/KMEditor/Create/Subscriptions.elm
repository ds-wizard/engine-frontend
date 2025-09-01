module Wizard.Pages.KMEditor.Create.Subscriptions exposing (subscriptions)

import Wizard.Components.TypeHintInput as TypeHintInput
import Wizard.Pages.KMEditor.Create.Models exposing (Model)
import Wizard.Pages.KMEditor.Create.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map PackageTypeHintInputMsg <|
        TypeHintInput.subscriptions model.packageTypeHintInputModel
