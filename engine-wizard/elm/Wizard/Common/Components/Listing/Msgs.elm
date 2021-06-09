module Wizard.Common.Components.Listing.Msgs exposing (Msg(..))

import Bootstrap.Dropdown as Dropdown
import Debouncer.Extra as Debouncer
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Shared.Error.ApiError exposing (ApiError)


type Msg a
    = ItemDropdownMsg Int Dropdown.State
    | SortDropdownMsg Dropdown.State
    | FilterDropdownMsg String Dropdown.State
    | Reload
    | ReloadBackground
    | GetItemsComplete PaginationQueryString (Result ApiError (Pagination a))
    | QueryInput String
    | QueryApply String
    | DebouncerMsg (Debouncer.Msg (Msg a))
