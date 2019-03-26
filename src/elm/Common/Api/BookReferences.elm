module Common.Api.BookReferences exposing (getBookReference)

import Common.Api exposing (ToMsg, httpGet)
import Common.AppState exposing (AppState)
import Public.BookReference.Models exposing (BookReference, bookReferenceDecoder)


getBookReference : String -> AppState -> ToMsg BookReference msg -> Cmd msg
getBookReference uuid =
    httpGet ("/book-references/" ++ uuid) bookReferenceDecoder
