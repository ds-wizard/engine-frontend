module Questionnaires.Create.Update exposing (..)

import Msgs
import Questionnaires.Create.Models exposing (Model)
import Questionnaires.Create.Msgs exposing (Msg)


update : Msg -> (Msg -> Msgs.Msg) -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg model =
    ( model, Cmd.none )
