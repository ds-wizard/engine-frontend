module Wizard.Registry.RegistrySignupConfirmation.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Shared.Api.Registry as RegistryApi
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Registry.RegistrySignupConfirmation.Models exposing (Model)
import Wizard.Registry.RegistrySignupConfirmation.Msgs exposing (Msg(..))


fetchData : String -> String -> AppState -> Cmd Msg
fetchData organizationId hash appState =
    RegistryApi.postConfirmation organizationId hash appState PostConfirmationComplete


update : Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg appState model =
    case msg of
        PostConfirmationComplete result ->
            ( handlePostConfirmationComplete appState model result, Cmd.none )


handlePostConfirmationComplete : AppState -> Model -> Result ApiError () -> Model
handlePostConfirmationComplete appState model result =
    case result of
        Ok _ ->
            { model | confirmation = Success () }

        Err error ->
            { model | confirmation = ApiError.toActionResult appState (gettext "Unable to confirm the registry account." appState.locale) error }
