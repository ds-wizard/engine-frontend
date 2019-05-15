module Public.BookReference.Update exposing
    ( fetchData
    , update
    )

import Common.Api exposing (applyResult)
import Common.Api.BookReferences as BookReferencesApi
import Common.AppState exposing (AppState)
import Common.Setters exposing (setBookReference)
import Msgs
import Public.BookReference.Models exposing (BookReference, Model)
import Public.BookReference.Msgs exposing (Msg(..))


fetchData : (Msg -> Msgs.Msg) -> String -> AppState -> Cmd Msgs.Msg
fetchData wrapMsg uuid appState =
    Cmd.map wrapMsg <|
        BookReferencesApi.getBookReference uuid appState GetBookReferenceCompleted


update : Msg -> (Msg -> Msgs.Msg) -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg model =
    case msg of
        GetBookReferenceCompleted result ->
            applyResult
                { setResult = setBookReference
                , defaultError = "Unable to get book reference."
                , model = model
                , result = result
                }
