module Wizard.Common.Components.Listing.Update exposing
    ( UpdateConfig
    , fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Browser.Navigation as Navigation
import Debouncer.Extra as Debouncer
import Dict
import List.Extra as List
import Set
import Shared.Api.Request exposing (ToMsg)
import Shared.Data.ApiError as ApiError
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Utils.RequestHelpers as RequestHelpers
import Shared.Utils.Setters exposing (setDropdownState)
import Task.Extra as Task
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Models exposing (Model, setPagination)
import Wizard.Common.Components.Listing.Msgs exposing (Msg(..))
import Wizard.Msgs
import Wizard.Routes exposing (Route)
import Wizard.Routing as Routing


type alias UpdateConfig a =
    { getRequest : PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination a) (Msg a) -> Cmd (Msg a)
    , getError : String
    , wrapMsg : Msg a -> Wizard.Msgs.Msg
    , toRoute : PaginationQueryFilters -> PaginationQueryString -> Route
    }


fetchData : Cmd (Msg a)
fetchData =
    Task.dispatch Reload


update : UpdateConfig a -> AppState -> Msg a -> Model a -> ( Model a, Cmd Wizard.Msgs.Msg )
update cfg appState msg model =
    let
        updatePagination mbFilterId pqf pqs =
            let
                loadCmd =
                    Cmd.map cfg.wrapMsg <| cfg.getRequest pqf pqs (GetItemsComplete False pqs pqf)

                replaceUrlCmd =
                    Navigation.replaceUrl appState.key (Routing.toUrl (cfg.toRoute pqf pqs))

                filterKeepOpen =
                    case mbFilterId of
                        Just filterId ->
                            Set.insert filterId model.filterKeepOpen

                        Nothing ->
                            model.filterKeepOpen
            in
            ( { model
                | pagination = Loading
                , items = []
                , paginationQueryString = pqs
                , filters = pqf
                , filterKeepOpen = filterKeepOpen
              }
            , Cmd.batch
                [ loadCmd
                , replaceUrlCmd
                ]
            )
    in
    case msg of
        ItemDropdownMsg index state ->
            ( { model | items = List.updateAt index (setDropdownState state) model.items }, Cmd.none )

        SortDropdownMsg state ->
            ( { model | sortDropdownState = state }, Cmd.none )

        FilterDropdownMsg filterId state ->
            if Set.member filterId model.filterKeepOpen then
                ( { model | filterKeepOpen = Set.remove filterId model.filterKeepOpen }, Cmd.none )

            else
                ( { model | filterDropdownStates = Dict.insert filterId state model.filterDropdownStates }, Cmd.none )

        Reload ->
            ( { model | pagination = Loading, items = [] }
            , Cmd.map cfg.wrapMsg <| cfg.getRequest model.filters model.paginationQueryString (GetItemsComplete False model.paginationQueryString model.filters)
            )

        ReloadBackground ->
            ( model
            , Cmd.map cfg.wrapMsg <| cfg.getRequest model.filters model.paginationQueryString (GetItemsComplete True model.paginationQueryString model.filters)
            )

        GetItemsComplete useOriginalState paginationQueryString paginationQueryFilters result ->
            case result of
                Ok pagination ->
                    if model.paginationQueryString == paginationQueryString && model.filters == paginationQueryFilters then
                        ( setPagination useOriginalState pagination model, Cmd.none )

                    else
                        ( model, Cmd.none )

                Err error ->
                    ( { model | pagination = ApiError.toActionResult appState cfg.getError error }
                    , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
                    )

        UpdatePaginationQueryString pqs ->
            updatePagination Nothing model.filters pqs

        UpdatePaginationQueryFilters mbFilterId pqf ->
            updatePagination mbFilterId pqf (PaginationQueryString.resetPage model.paginationQueryString)

        QueryInput string ->
            ( { model | qInput = string }
            , Task.dispatch (cfg.wrapMsg <| DebouncerMsg <| Debouncer.provideInput <| QueryApply string)
            )

        QueryApply string ->
            let
                paginationQueryString =
                    model.paginationQueryString

                newPaginationQueryString =
                    { paginationQueryString | q = Just string, page = Just 1 }
            in
            updatePagination Nothing model.filters newPaginationQueryString

        DebouncerMsg debounceMsg ->
            let
                updateConfig =
                    { mapMsg = cfg.wrapMsg << DebouncerMsg
                    , getDebouncer = .qDebouncer
                    , setDebouncer = \debouncer m -> { m | qDebouncer = debouncer }
                    }
            in
            Debouncer.update (update cfg appState) updateConfig debounceMsg model

        OnAfterDelete ->
            let
                paginationQueryString =
                    case model.pagination of
                        Success pagination ->
                            let
                                isLastPage =
                                    pagination.page.number == (pagination.page.totalPages - 1)

                                isFirstPage =
                                    pagination.page.number == 0

                                isLastElement =
                                    modBy pagination.page.size pagination.page.totalElements == 1
                            in
                            if (isLastPage && not isFirstPage) && isLastElement then
                                PaginationQueryString.setPage model.paginationQueryString pagination.page.number

                            else
                                model.paginationQueryString

                        _ ->
                            model.paginationQueryString
            in
            updatePagination Nothing model.filters paginationQueryString
