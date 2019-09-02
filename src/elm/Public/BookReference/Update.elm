module Public.BookReference.Update exposing
    ( fetchData
    , update
    )

import Common.Api exposing (applyResult)
import Common.Api.BookReferences as BookReferencesApi
import Common.AppState exposing (AppState)
import Common.Locale exposing (lg)
import Common.Setters exposing (setBookReference)
import Msgs
import Public.BookReference.Models exposing (Model)
import Public.BookReference.Msgs exposing (Msg(..))


fetchData : String -> AppState -> Cmd Msg
fetchData uuid appState =
    BookReferencesApi.getBookReference uuid appState GetBookReferenceCompleted


update : Msg -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg appState model =
    case msg of
        GetBookReferenceCompleted result ->
            applyResult
                { setResult = setBookReference
                , defaultError = lg "apiError.bookReferences.getError" appState
                , model = model
                , result = result
                }
