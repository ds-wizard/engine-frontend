module Wizard.Dev.PersistentCommandsIndex.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Shared.Api.PersistentCommands as PersistentCommandsApi
import Shared.Data.PaginationQueryFilters as PaginationQueryFilters
import Shared.Data.PersistentCommand exposing (PersistentCommand)
import Shared.Error.ApiError as ApiError
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Update as Listing
import Wizard.Dev.PersistentCommandsIndex.Models exposing (Model)
import Wizard.Dev.PersistentCommandsIndex.Msgs exposing (Msg(..))
import Wizard.Dev.Routes exposing (persistentCommandIndexRouteStateFilterId)
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
                    Listing.update (listingUpdateConfig wrapMsg model) appState listingMsg model.persistentCommands
            in
            ( { model | persistentCommands = persistentCommands }
            , cmd
            )

        RetryFailed ->
            ( { model | retryFailed = Loading }
            , Cmd.map wrapMsg (PersistentCommandsApi.retryAllFailed appState RetryFailedComplete)
            )

        RetryFailedComplete result ->
            case result of
                Ok _ ->
                    ( model, Ports.refresh () )

                Err error ->
                    ( { model | retryFailed = ApiError.toActionResult appState "Unable to retry all failed persistent commands." error }
                    , Cmd.none
                    )


listingUpdateConfig : (Msg -> Wizard.Msgs.Msg) -> Model -> Listing.UpdateConfig PersistentCommand
listingUpdateConfig wrapMsg model =
    let
        state =
            PaginationQueryFilters.getValue persistentCommandIndexRouteStateFilterId model.persistentCommands.filters
    in
    { getRequest = PersistentCommandsApi.getPersistentCommands { state = state }
    , getError = "Unable to get persistent commands"
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.persistentCommandsIndexWithFilters PaginationQueryFilters.empty
    }
