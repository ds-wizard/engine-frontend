module Wizard.Pages.Dev.PersistentCommandsIndex.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Common.Data.ApiError as ApiError
import Common.Data.PersistentCommandState as PersistentCommandState
import Common.Utils.RequestHelpers as RequestHelpers
import Task.Extra as Task
import Wizard.Api.Models.PersistentCommand exposing (PersistentCommand)
import Wizard.Api.PersistentCommands as PersistentCommandsApi
import Wizard.Components.Listing.Msgs as ListingMsgs
import Wizard.Components.Listing.Update as Listing
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Dev.PersistentCommandsIndex.Models exposing (Model)
import Wizard.Pages.Dev.PersistentCommandsIndex.Msgs exposing (Msg(..))
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
