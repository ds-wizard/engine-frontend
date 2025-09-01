module Wizard.Pages.Registry.Update exposing (fetchData, update)

import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Registry.Models exposing (Model)
import Wizard.Pages.Registry.Msgs exposing (Msg(..))
import Wizard.Pages.Registry.RegistrySignupConfirmation.Update
import Wizard.Pages.Registry.Routes exposing (Route(..))


fetchData : Route -> AppState -> Cmd Msg
fetchData route appState =
    case route of
        RegistrySignupConfirmationRoute organizationId hash ->
            Cmd.map RegistrySignupConfirmationMsg <|
                Wizard.Pages.Registry.RegistrySignupConfirmation.Update.fetchData organizationId hash appState


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg _ appState model =
    case msg of
        RegistrySignupConfirmationMsg rMsg ->
            let
                ( registrySignupConfirmationModel, cmd ) =
                    Wizard.Pages.Registry.RegistrySignupConfirmation.Update.update rMsg appState model.registrySignupConfirmationModel
            in
            ( { model | registrySignupConfirmationModel = registrySignupConfirmationModel }, cmd )
