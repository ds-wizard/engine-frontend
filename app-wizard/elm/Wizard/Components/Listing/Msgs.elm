module Wizard.Components.Listing.Msgs exposing (Msg(..))

import Bootstrap.Dropdown as Dropdown
import Common.Data.ApiError exposing (ApiError)
import Common.Data.Pagination exposing (Pagination)
import Common.Data.PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Debouncer.Extra as Debouncer


type Msg a
    = ItemDropdownMsg Int Dropdown.State
    | SortDropdownMsg Dropdown.State
    | FilterDropdownMsg String Dropdown.State
    | Reload
    | ReloadBackground
    | GetItemsComplete Bool PaginationQueryString PaginationQueryFilters (Result ApiError (Pagination a))
    | UpdatePaginationQueryString PaginationQueryString
    | UpdatePaginationQueryFilters (Maybe String) PaginationQueryFilters
    | QueryInput String
    | QueryApply String
    | DebouncerMsg (Debouncer.Msg (Msg a))
    | OnAfterDelete
