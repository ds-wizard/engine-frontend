module Common.Api.BookReferences exposing (getBookReference)

import Common.Api exposing (ToMsg, httpGet)
import Common.AppState exposing (AppState)
import Public.Common.BookReference as BookReference exposing (BookReference)


getBookReference : String -> AppState -> ToMsg BookReference msg -> Cmd msg
getBookReference uuid =
    httpGet ("/book-references/" ++ uuid) BookReference.decoder
