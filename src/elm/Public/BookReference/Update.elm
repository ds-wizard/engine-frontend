module Public.BookReference.Update exposing (fetchData, handleGetBookReferenceCompleted, update)

import ActionResult exposing (ActionResult(..))
import Common.Api.BookReferences as BookReferencesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
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
            handleGetBookReferenceCompleted model result


handleGetBookReferenceCompleted : Model -> Result ApiError BookReference -> ( Model, Cmd Msgs.Msg )
handleGetBookReferenceCompleted model result =
    let
        newModel =
            case result of
                Ok bookReference ->
                    { model | bookReference = Success bookReference }

                Err error ->
                    { model | bookReference = getServerError error "Unable to get book reference" }
    in
    ( newModel, Cmd.none )
