module Wizard.Common.Components.Listing.Msgs exposing (Msg(..))

import Bootstrap.Dropdown as Dropdown
import Debouncer.Extra as Debouncer
import Shared.Data.ApiError exposing (ApiError)
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)


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
