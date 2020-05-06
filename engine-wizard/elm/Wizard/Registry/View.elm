module Wizard.Registry.View exposing (view)

import Html exposing (Html)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Registry.Models exposing (Model)
import Wizard.Registry.Msgs exposing (Msg(..))
import Wizard.Registry.RegistrySignupConfirmation.View
import Wizard.Registry.Routes exposing (Route(..))


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        RegistrySignupConfirmationRoute _ _ ->
            Html.map RegistrySignupConfirmationMsg <|
                Wizard.Registry.RegistrySignupConfirmation.View.view appState model.registrySignupConfirmationModel
