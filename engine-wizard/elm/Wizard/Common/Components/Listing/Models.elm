module Wizard.Common.Components.Listing.Models exposing
    ( Item
    , Model
    , initialModel
    , setPagination
    )

import ActionResult exposing (ActionResult(..))
import Bootstrap.Dropdown as Dropdown
import Debouncer.Extra as Debouncer exposing (Debouncer)
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
    }


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
