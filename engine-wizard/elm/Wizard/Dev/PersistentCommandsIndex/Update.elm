module Wizard.Dev.PersistentCommandsIndex.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Shared.Api.PersistentCommands as PersistentCommandsApi
import Shared.Data.PersistentCommand exposing (PersistentCommand)
import Shared.Data.PersistentCommand.PersistentCommandState as PersistentCommandState
import Shared.Error.ApiError as ApiError
import Shared.Utils exposing (dispatch)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Msgs as ListingMsgs
import Wizard.Common.Components.Listing.Update as Listing
import Wizard.Dev.PersistentCommandsIndex.Models exposing (Model)
import Wizard.Dev.PersistentCommandsIndex.Msgs exposing (Msg(..))
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Routes as Routes


fetchData : Cmd Msg
fetchData =
    Cmd.map ListingMsg Listing.fetchData


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        ListingMsg listingMsg ->
            let
                ( persistentCommands, cmd ) =
                    Listing.update (listingUpdateConfig wrapMsg) appState listingMsg model.persistentCommands
            in
            ( { model | persistentCommands = persistentCommands }
            , cmd
            )

        RetryFailed ->
            ( { model | updating = Loading }
            , Cmd.map wrapMsg (PersistentCommandsApi.retryAllFailed appState RetryFailedComplete)
            )

        RetryFailedComplete result ->
            case result of
                Ok _ ->
                    ( model, Ports.refresh () )

                Err error ->
                    ( { model | updating = ApiError.toActionResult appState "Unable to retry all failed persistent commands." error }
                    , Cmd.none
                    )

        RerunCommand persistentCommand ->
            ( model
            , PersistentCommandsApi.retry persistentCommand.uuid appState (wrapMsg << UpdateComplete)
            )

        SetIgnored persistentCommand ->
            ( model
            , PersistentCommandsApi.updateState persistentCommand.uuid PersistentCommandState.Ignore appState (wrapMsg << UpdateComplete)
            )

        UpdateComplete result ->
            case result of
                Ok _ ->
                    ( model
                    , dispatch (wrapMsg (ListingMsg ListingMsgs.OnAfterDelete))
                    )

                Err error ->
                    ( { model | updating = ApiError.toActionResult appState "Persistent command update failed." error }
                    , getResultCmd Wizard.Msgs.logoutMsg result
                    )


listingUpdateConfig : (Msg -> Wizard.Msgs.Msg) -> Listing.UpdateConfig PersistentCommand
listingUpdateConfig wrapMsg =
    { getRequest = PersistentCommandsApi.getPersistentCommands
    , getError = "Unable to get persistent commands"
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.persistentCommandsIndexWithFilters
    }
