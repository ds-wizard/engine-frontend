module Questionnaires.Index.Update exposing (..)

import Auth.Models exposing (Session)
import Common.Types exposing (ActionResult(..))
import Jwt
import Msgs
import Questionnaires.Common.Models exposing (Questionnaire)
import Questionnaires.Index.Models exposing (Model)
import Questionnaires.Index.Msgs exposing (Msg(GetQuestionnairesCompleted))
import Questionnaires.Requests exposing (getQuestionnaires)


fetchData : (Msg -> Msgs.Msg) -> Session -> Cmd Msgs.Msg
fetchData wrapMsg session =
    getQuestionnairesCmd session |> Cmd.map wrapMsg


update : Msg -> (Msg -> Msgs.Msg) -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg model =
    case msg of
        GetQuestionnairesCompleted result ->
            getQuestionnairesCompleted model result

        _ ->
            ( model, Cmd.none )


getQuestionnairesCmd : Session -> Cmd Msg
getQuestionnairesCmd =
    getQuestionnaires >> Jwt.send GetQuestionnairesCompleted


getQuestionnairesCompleted : Model -> Result Jwt.JwtError (List Questionnaire) -> ( Model, Cmd Msgs.Msg )
getQuestionnairesCompleted model result =
    let
        newModel =
            case result of
                Ok users ->
                    { model | questionnaires = Success users }

                Err error ->
                    { model | questionnaires = Error "Unable to fetch questionnaire list" }
    in
    ( newModel, Cmd.none )
