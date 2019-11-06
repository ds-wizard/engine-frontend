module Wizard.Public.BookReference.Msgs exposing (Msg(..))

import Shared.Error.ApiError exposing (ApiError)
import Wizard.Public.Common.BookReference exposing (BookReference)


type Msg
    = GetBookReferenceCompleted (Result ApiError BookReference)
