module Wizard.Public.BookReference.Msgs exposing (Msg(..))

import Shared.Data.BookReference exposing (BookReference)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = GetBookReferenceCompleted (Result ApiError BookReference)
