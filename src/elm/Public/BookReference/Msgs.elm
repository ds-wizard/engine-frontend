module Public.BookReference.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Public.Common.BookReference exposing (BookReference)


type Msg
    = GetBookReferenceCompleted (Result ApiError BookReference)
