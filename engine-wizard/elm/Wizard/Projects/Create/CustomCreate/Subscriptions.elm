module Wizard.Projects.Create.CustomCreate.Subscriptions exposing (subscriptions)

import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Projects.Create.CustomCreate.Models exposing (Model)
import Wizard.Projects.Create.CustomCreate.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map PackageTypeHintInputMsg <|
        TypeHintInput.subscriptions model.packageTypeHintInputModel
