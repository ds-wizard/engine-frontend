module Wizard.Public.BookReference.Update exposing
    ( fetchData
    , update
    )

import Gettext exposing (gettext)
import Shared.Api.BookReferences as BookReferencesApi
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
                , defaultError = gettext "Unable to get a book reference." appState.locale
                , model = model
                , result = result
                }
