module Wizard.Common.Api.BookReferences exposing (getBookReference)

import Wizard.Common.Api exposing (ToMsg, httpGet)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Public.Common.BookReference as BookReference exposing (BookReference)


getBookReference : String -> AppState -> ToMsg BookReference msg -> Cmd msg
getBookReference uuid =
    httpGet ("/book-references/" ++ uuid) BookReference.decoder
