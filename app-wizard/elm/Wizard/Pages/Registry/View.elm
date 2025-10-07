module Wizard.Pages.Registry.View exposing (view)

import Html exposing (Html)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Registry.Models exposing (Model)
import Wizard.Pages.Registry.Msgs exposing (Msg(..))
import Wizard.Pages.Registry.RegistrySignupConfirmation.View
import Wizard.Pages.Registry.Routes exposing (Route)


view : Route -> AppState -> Model -> Html Msg
view _ appState model =
    Html.map RegistrySignupConfirmationMsg <|
        Wizard.Pages.Registry.RegistrySignupConfirmation.View.view appState model.registrySignupConfirmationModel
