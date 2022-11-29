module Wizard.Common.Components.Listing.Update exposing (UpdateConfig, fetchData, update)

import ActionResult exposing (ActionResult(..))
import Browser.Dom as Dom
import Browser.Navigation as Navigation
import Debouncer.Extra as Debouncer
import Dict
import List.Extra as List
import Shared.Api exposing (ToMsg)
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Shared.Error.ApiError as ApiError
import Shared.Setters exposing (setDropdownState)
import Shared.Utils exposing (dispatch)
import Task
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Models exposing (Model, setPagination)
import Wizard.Common.Components.Listing.Msgs exposing (Msg(..))
import Wizard.Msgs
import Wizard.Routes exposing (Route)
import Wizard.Routing as Routing


type alias UpdateConfig a =
    { getRequest : PaginationQueryString -> AppState -> ToMsg (Pagination a) (Msg a) -> Cmd (Msg a)
    , getError : String
    , wrapMsg : Msg a -> Wizard.Msgs.Msg
    , toRoute : PaginationQueryString -> Route
    }


fetchData : Cmd (Msg a)
fetchData =
    dispatch Reload


update : UpdateConfig a -> AppState -> Msg a -> Model a -> ( Model a, Cmd Wizard.Msgs.Msg )
update cfg appState msg model =
    case msg of
        ItemDropdownMsg index state ->
            ( { model | items = List.updateAt index (setDropdownState state) model.items }, Cmd.none )

        SortDropdownMsg state ->
            ( { model | sortDropdownState = state }, Cmd.none )

        FilterDropdownMsg filterId state ->
            ( { model | filterDropdownStates = Dict.insert filterId state model.filterDropdownStates }, Cmd.none )

        Reload ->
            ( { model | pagination = Loading, items = [] }
            , Cmd.map cfg.wrapMsg <| cfg.getRequest model.paginationQueryString appState (GetItemsComplete model.paginationQueryString)
            )

        ReloadBackground ->
            ( model
            , Cmd.map cfg.wrapMsg <| cfg.getRequest model.paginationQueryString appState (GetItemsComplete model.paginationQueryString)
            )

        GetItemsComplete paginationQueryString result ->
            case result of
                Ok pagination ->
                    if model.paginationQueryString == paginationQueryString then
                        let
                            cmd =
                                if String.isEmpty model.qInput then
                                    Cmd.none

                                else
                                    Task.attempt (always (cfg.wrapMsg NoOp)) (Dom.focus "filter")
                        in
                        ( setPagination pagination model, cmd )

                    else
                        ( model, Cmd.none )

                Err error ->
                    ( { model | pagination = ApiError.toActionResult appState cfg.getError error }
                    , getResultCmd Wizard.Msgs.logoutMsg result
                    )

        QueryInput string ->
            ( { model | qInput = string }
            , dispatch (cfg.wrapMsg <| DebouncerMsg <| Debouncer.provideInput <| QueryApply string)
            )

        QueryApply string ->
            let
                paginationQueryString =
                    model.paginationQueryString

                url =
                    Routing.toUrl appState <|
                        cfg.toRoute { paginationQueryString | q = Just string, page = Just 1 }
            in
            ( { model | pagination = Loading, items = [] }
            , Navigation.replaceUrl appState.key url
            )

        DebouncerMsg debounceMsg ->
            let
                updateConfig =
                    { mapMsg = cfg.wrapMsg << DebouncerMsg
                    , getDebouncer = .qDebouncer
                    , setDebouncer = \debouncer m -> { m | qDebouncer = debouncer }
                    }
            in
            Debouncer.update (update cfg appState) updateConfig debounceMsg model

        NoOp ->
            ( model, Cmd.none )
