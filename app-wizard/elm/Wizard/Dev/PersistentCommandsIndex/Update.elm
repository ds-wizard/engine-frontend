module Wizard.Dev.PersistentCommandsIndex.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Shared.Data.ApiError as ApiError
import Shared.Utils.RequestHelpers as RequestHelpers
import Task.Extra as Task
import Wizard.Api.Models.PersistentCommand exposing (PersistentCommand)
import Wizard.Api.Models.PersistentCommand.PersistentCommandState as PersistentCommandState
import Wizard.Api.PersistentCommands as PersistentCommandsApi
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
                    Listing.update (listingUpdateConfig wrapMsg appState) appState listingMsg model.persistentCommands
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
            , PersistentCommandsApi.retry appState persistentCommand.uuid (wrapMsg << UpdateComplete)
            )

        SetIgnored persistentCommand ->
            ( model
            , PersistentCommandsApi.updateState appState persistentCommand.uuid PersistentCommandState.Ignore (wrapMsg << UpdateComplete)
            )

        UpdateComplete result ->
            case result of
                Ok _ ->
                    ( model
                    , Task.dispatch (wrapMsg (ListingMsg ListingMsgs.OnAfterDelete))
                    )

                Err error ->
                    ( { model | updating = ApiError.toActionResult appState "Persistent command update failed." error }
                    , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
                    )


listingUpdateConfig : (Msg -> Wizard.Msgs.Msg) -> AppState -> Listing.UpdateConfig PersistentCommand
listingUpdateConfig wrapMsg appState =
    { getRequest = PersistentCommandsApi.getPersistentCommands appState
    , getError = "Unable to get persistent commands"
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.persistentCommandsIndexWithFilters
    }
