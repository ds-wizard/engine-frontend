module Public.BookReference.Requests exposing (getBookReference)

import Http
import Public.BookReference.Models exposing (BookReference, bookReferenceDecoder)
import Requests exposing (apiUrl)


getBookReference : String -> Http.Request BookReference
getBookReference uuid =
    Http.get (apiUrl "/book-references/" ++ uuid) bookReferenceDecoder
