module KMEditor.Editor2.Update exposing (fetchData, update)

import Auth.Models exposing (Session)
import KMEditor.Editor2.Models exposing (Model)
import KMEditor.Editor2.Msgs exposing (Msg)
import Models exposing (State)
import Msgs
import Random exposing (Seed)


fetchData : (Msg -> Msgs.Msg) -> String -> Session -> Cmd Msgs.Msg
fetchData wrapMsg uuid session =
    Cmd.none


update : Msg -> (Msg -> Msgs.Msg) -> State -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg wrapMsg state model =
    ( state.seed, model, Cmd.none )
