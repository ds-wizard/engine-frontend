module Public.BookReference.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Public.BookReference.Models exposing (BookReference)


type Msg
    = GetBookReferenceCompleted (Result ApiError BookReference)
