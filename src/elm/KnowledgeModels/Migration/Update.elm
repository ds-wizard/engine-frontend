module KnowledgeModels.Migration.Update exposing (..)

import Auth.Models exposing (Session)
import KnowledgeModels.Migration.Models exposing (Model)
import KnowledgeModels.Migration.Msgs exposing (Msg)
import Msgs
import Random.Pcg exposing (Seed)


update : Msg -> Seed -> Session -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg seed session model =
    ( seed, model, Cmd.none )
