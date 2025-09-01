module Wizard.Registry.Update exposing (fetchData, update)

import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Registry.Models exposing (Model)
import Wizard.Registry.Msgs exposing (Msg(..))
import Wizard.Registry.RegistrySignupConfirmation.Update
import Wizard.Registry.Routes exposing (Route(..))


fetchData : Route -> AppState -> Cmd Msg
fetchData route appState =
    case route of
        RegistrySignupConfirmationRoute organizationId hash ->
            Cmd.map RegistrySignupConfirmationMsg <|
                Wizard.Registry.RegistrySignupConfirmation.Update.fetchData organizationId hash appState


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg _ appState model =
    case msg of
        RegistrySignupConfirmationMsg rMsg ->
            let
                ( registrySignupConfirmationModel, cmd ) =
                    Wizard.Registry.RegistrySignupConfirmation.Update.update rMsg appState model.registrySignupConfirmationModel
            in
            ( { model | registrySignupConfirmationModel = registrySignupConfirmationModel }, cmd )
