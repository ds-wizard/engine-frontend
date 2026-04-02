module Wizard.Pages.Dev.Operations.Subscriptions exposing (subscriptions)

import Common.Components.TypeHintInput as TypeHintInput
import Dict
import Wizard.Pages.Dev.Operations.Models exposing (Model)
import Wizard.Pages.Dev.Operations.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        typeHintInputSubs =
            Dict.toList model.typeHintInputModels
                |> List.map (\( path, typeHintInputModel ) -> Sub.map (UpdateTypeHintInput path) (TypeHintInput.subscriptions typeHintInputModel))
    in
    Sub.batch typeHintInputSubs
