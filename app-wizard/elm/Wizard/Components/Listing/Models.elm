module Wizard.Components.Listing.Models exposing
    ( Item
    , Model
    , initialModel
    , initialModelWithFilters
    , initialModelWithFiltersAndStates
    , setPagination
    , updateItems
    )

import ActionResult exposing (ActionResult(..))
import Bootstrap.Dropdown as Dropdown
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Debouncer.Extra as Debouncer exposing (Debouncer)
import Dict exposing (Dict)
import List.Extra as List
import Maybe.Extra as Maybe
import Set exposing (Set)
import Wizard.Components.Listing.Msgs exposing (Msg)


type alias Model a =
    { pagination : ActionResult (Pagination a)
    , paginationQueryString : PaginationQueryString
    , items : List (Item a)
    , qInput : String
    , qDebouncer : Debouncer (Msg a)
    , sortDropdownState : Dropdown.State
    , filters : PaginationQueryFilters
    , filterDropdownStates : Dict String Dropdown.State
    , filterKeepOpen : Set String
    }


type alias Item a =
    { dropdownState : Dropdown.State
    , item : a
    }


initialModel : PaginationQueryString -> Model a
initialModel paginationQueryString =
    { pagination = Loading
    , paginationQueryString = paginationQueryString
    , items = []
    , qInput = Maybe.withDefault "" paginationQueryString.q
    , qDebouncer = Debouncer.toDebouncer <| Debouncer.debounce 500
    , sortDropdownState = Dropdown.initialState
    , filters = PaginationQueryFilters.empty
    , filterDropdownStates = Dict.empty
    , filterKeepOpen = Set.empty
    }


initialModelWithFilters : PaginationQueryString -> PaginationQueryFilters -> Model a
initialModelWithFilters paginationQueryString filters =
    { pagination = Loading
    , paginationQueryString = paginationQueryString
    , items = []
    , qInput = Maybe.withDefault "" paginationQueryString.q
    , qDebouncer = Debouncer.toDebouncer <| Debouncer.debounce 500
    , sortDropdownState = Dropdown.initialState
    , filters = filters
    , filterDropdownStates = Dict.empty
    , filterKeepOpen = Set.empty
    }


initialModelWithFiltersAndStates : PaginationQueryString -> PaginationQueryFilters -> Maybe (Model a) -> Model a
initialModelWithFiltersAndStates paginationQueryString filters oldModel =
    { pagination = Loading
    , paginationQueryString = paginationQueryString
    , items = []
    , qInput = Maybe.withDefault "" paginationQueryString.q
    , qDebouncer = Debouncer.toDebouncer <| Debouncer.debounce 500
    , sortDropdownState = Dropdown.initialState
    , filters = filters
    , filterDropdownStates = Maybe.unwrap Dict.empty .filterDropdownStates oldModel
    , filterKeepOpen = Set.empty
    }


updateItems : (a -> a) -> Model a -> Model a
updateItems updateItem model =
    { model | items = List.map (\item -> { item | item = updateItem item.item }) model.items }


setPagination : Bool -> Pagination a -> Model a -> Model a
setPagination useOriginalState pagination model =
    let
        getState item =
            if useOriginalState then
                List.find (\i -> i.item == item) model.items
                    |> Maybe.unwrap Dropdown.initialState .dropdownState

            else
                Dropdown.initialState

        wrap item =
            { dropdownState = getState item
            , item = item
            }
    in
    { model
        | pagination = Success pagination
        , items = List.map wrap pagination.items
    }
