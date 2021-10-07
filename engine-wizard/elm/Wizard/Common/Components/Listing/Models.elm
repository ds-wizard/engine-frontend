module Wizard.Common.Components.Listing.Models exposing
    ( Item
    , Model
    , initialModel
    , initialModelWithFilters
    , setPagination
    , updateItems
    )

import ActionResult exposing (ActionResult(..))
import Bootstrap.Dropdown as Dropdown
import Debouncer.Extra as Debouncer exposing (Debouncer)
import Dict exposing (Dict)
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Wizard.Common.Components.Listing.Msgs exposing (Msg)


type alias Model a =
    { pagination : ActionResult (Pagination a)
    , paginationQueryString : PaginationQueryString
    , items : List (Item a)
    , qInput : String
    , qDebouncer : Debouncer (Msg a)
    , sortDropdownState : Dropdown.State
    , filters : Dict String String
    , filterDropdownStates : Dict String Dropdown.State
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
    , filters = Dict.empty
    , filterDropdownStates = Dict.empty
    }


initialModelWithFilters : PaginationQueryString -> Dict String String -> Model a
initialModelWithFilters paginationQueryString filters =
    { pagination = Loading
    , paginationQueryString = paginationQueryString
    , items = []
    , qInput = Maybe.withDefault "" paginationQueryString.q
    , qDebouncer = Debouncer.toDebouncer <| Debouncer.debounce 500
    , sortDropdownState = Dropdown.initialState
    , filters = filters
    , filterDropdownStates = Dict.empty
    }


updateItems : (a -> a) -> Model a -> Model a
updateItems updateItem model =
    { model | items = List.map (\item -> { item | item = updateItem item.item }) model.items }


setPagination : Pagination a -> Model a -> Model a
setPagination pagination model =
    let
        wrap item =
            { dropdownState = Dropdown.initialState
            , item = item
            }
    in
    { model
        | pagination = Success pagination
        , items = List.map wrap pagination.items
    }
