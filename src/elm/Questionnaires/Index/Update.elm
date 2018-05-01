module Questionnaires.Index.Update exposing (..)

import Msgs
import Questionnaires.Index.Models exposing (Model)
import Questionnaires.Index.Msgs exposing (Msg)


update : Msg -> (Msg -> Msgs.Msg) -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg model =
    ( model, Cmd.none )
