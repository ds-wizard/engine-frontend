module KnowledgeModels.Index.Update exposing (..)

import Auth.Models exposing (Session)
import Jwt
import KnowledgeModels.Index.Models exposing (Model)
import KnowledgeModels.Index.Msgs exposing (Msg(..))
import KnowledgeModels.Models exposing (KnowledgeModel)
import KnowledgeModels.Requests exposing (getKnowledgeModels)
import Msgs
import Requests exposing (toCmd)


getKnowledgeModelsCmd : Session -> Cmd Msgs.Msg
getKnowledgeModelsCmd session =
    getKnowledgeModels session
        |> toCmd GetKnowledgeModelsCompleted Msgs.KnowledgeModelsIndexMsg


getKnowledgeModelsCompleted : Model -> Result Jwt.JwtError (List KnowledgeModel) -> ( Model, Cmd Msgs.Msg )
getKnowledgeModelsCompleted model result =
    let
        newModel =
            case result of
                Ok knowledgeModels ->
                    { model | knowledgeModels = knowledgeModels }

                Err error ->
                    { model | error = "Unable to fetch knowledge models" }
    in
    ( { newModel | loading = False }, Cmd.none )


update : Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg session model =
    case msg of
        GetKnowledgeModelsCompleted result ->
            getKnowledgeModelsCompleted model result
