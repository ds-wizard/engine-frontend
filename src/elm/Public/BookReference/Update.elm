module Public.BookReference.Update exposing (..)

import ActionResult exposing (ActionResult(..))
import Common.Models exposing (getServerError)
import Http
import Msgs
import Public.BookReference.Models exposing (BookReference, Model)
import Public.BookReference.Msgs exposing (Msg(..))
import Public.BookReference.Requests exposing (getBookReference)


fetchData : (Msg -> Msgs.Msg) -> String -> Cmd Msgs.Msg
fetchData wrapMsg uuid =
    getBookReference uuid
        |> Http.send GetBookReferenceCompleted
        |> Cmd.map wrapMsg


update : Msg -> (Msg -> Msgs.Msg) -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg model =
    case msg of
        GetBookReferenceCompleted result ->
            handleGetBookReferenceCompleted model result


handleGetBookReferenceCompleted : Model -> Result Http.Error BookReference -> ( Model, Cmd Msgs.Msg )
handleGetBookReferenceCompleted model result =
    let
        newModel =
            case result of
                Ok bookReference ->
                    { model | bookReference = Success bookReference }

                Err error ->
                    let
                        a =
                            Debug.log "error" error
                    in
                    { model | bookReference = getServerError error "Unable to get book reference" }
    in
    ( newModel, Cmd.none )
