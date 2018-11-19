module Public.BookReference.Msgs exposing (Msg(..))

import Http
import Public.BookReference.Models exposing (BookReference)


type Msg
    = GetBookReferenceCompleted (Result Http.Error BookReference)
