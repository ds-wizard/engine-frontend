module Wizard.Common.Components.Listing.Msgs exposing (Msg(..))

import Bootstrap.Dropdown as Dropdown
import Debouncer.Extra as Debouncer
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Pagination.Pagination exposing (Pagination)
import Wizard.Common.Pagination.PaginationQueryString exposing (PaginationQueryString)


type Msg a
    = ItemDropdownMsg Int Dropdown.State
    | SortDropdownMsg Dropdown.State
    | Reload
    | ReloadBackground
    | GetItemsComplete PaginationQueryString (Result ApiError (Pagination a))
    | QueryInput String
    | QueryApply String
    | DebouncerMsg (Debouncer.Msg (Msg a))
