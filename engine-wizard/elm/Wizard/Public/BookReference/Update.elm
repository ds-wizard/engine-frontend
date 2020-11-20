module Wizard.Public.BookReference.Update exposing
    ( fetchData
    , update
    )

import Shared.Api.BookReferences as BookReferencesApi
import Shared.Locale exposing (lg)
import Shared.Setters exposing (setBookReference)
import Wizard.Common.Api exposing (applyResult)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Public.BookReference.Models exposing (Model)
import Wizard.Public.BookReference.Msgs exposing (Msg(..))


fetchData : String -> AppState -> Cmd Msg
fetchData uuid appState =
    BookReferencesApi.getBookReference uuid appState GetBookReferenceCompleted


update : Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg appState model =
    case msg of
        GetBookReferenceCompleted result ->
            applyResult appState
                { setResult = setBookReference
                , defaultError = lg "apiError.bookReferences.getError" appState
                , model = model
                , result = result
                }
