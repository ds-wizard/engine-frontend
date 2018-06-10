module Public.BookReference.Msgs exposing (..)

import Http
import Public.BookReference.Models exposing (BookReference)


type Msg
    = GetBookReferenceCompleted (Result Http.Error BookReference)
