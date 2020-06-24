module Shared.Api.BookReferences exposing (getBookReference)

import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, httpGet)
import Shared.Data.BookReference as BookReference exposing (BookReference)


getBookReference : String -> AbstractAppState a -> ToMsg BookReference msg -> Cmd msg
getBookReference uuid =
    httpGet ("/book-references/" ++ uuid) BookReference.decoder
