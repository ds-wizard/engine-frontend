module Wizard.Projects.CreateMigration.Subscriptions exposing (subscriptions)

import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Projects.CreateMigration.Models exposing (Model)
import Wizard.Projects.CreateMigration.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map PackageTypeHintInputMsg <|
        TypeHintInput.subscriptions model.packageTypeHintInputModel
