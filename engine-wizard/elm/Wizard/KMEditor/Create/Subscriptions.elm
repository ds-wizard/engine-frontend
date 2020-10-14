module Wizard.KMEditor.Create.Subscriptions exposing (..)

import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.KMEditor.Create.Models exposing (Model)
import Wizard.KMEditor.Create.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map PackageTypeHintInputMsg <|
        TypeHintInput.subscriptions model.packageTypeHintInputModel
