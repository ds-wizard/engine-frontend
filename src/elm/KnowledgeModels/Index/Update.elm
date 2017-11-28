module KnowledgeModels.Index.Update exposing (..)

import Auth.Models exposing (Session)
import Common.Types exposing (ActionResult(..))
import Jwt
import KnowledgeModels.Index.Models exposing (Model)
import KnowledgeModels.Index.Msgs exposing (Msg(..))
import KnowledgeModels.Models exposing (KnowledgeModel)
import KnowledgeModels.Requests exposing (deleteKnowledgeModel, getKnowledgeModels, postMigration)
import Msgs
import Requests exposing (toCmd)
import Routing exposing (Route(..), cmdNavigate)


getKnowledgeModelsCmd : Session -> Cmd Msgs.Msg
getKnowledgeModelsCmd session =
    getKnowledgeModels session
        |> toCmd GetKnowledgeModelsCompleted Msgs.KnowledgeModelsIndexMsg


deleteKnowledgeModelCmd : String -> Session -> Cmd Msgs.Msg
deleteKnowledgeModelCmd kmId session =
    deleteKnowledgeModel kmId session
        |> toCmd DeleteKnowledgeModelCompleted Msgs.KnowledgeModelsIndexMsg


postMigrationCmd : String -> Session -> Cmd Msgs.Msg
postMigrationCmd uuid session =
    postMigration session uuid
        |> toCmd PostMigrationCompleted Msgs.KnowledgeModelsIndexMsg


getKnowledgeModelsCompleted : Model -> Result Jwt.JwtError (List KnowledgeModel) -> ( Model, Cmd Msgs.Msg )
getKnowledgeModelsCompleted model result =
    let
        newModel =
            case result of
                Ok knowledgeModels ->
                    { model | knowledgeModels = Success knowledgeModels }

                Err error ->
                    { model | knowledgeModels = Error "Unable to fetch knowledge models" }
    in
    ( newModel, Cmd.none )


handleDeleteKM : Session -> Model -> ( Model, Cmd Msgs.Msg )
handleDeleteKM session model =
    case model.kmToBeDeleted of
        Just km ->
            ( { model | deletingKnowledgeModel = Loading }
            , deleteKnowledgeModelCmd km.uuid session
            )

        _ ->
            ( model, Cmd.none )


deleteKnowledgeModelCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
deleteKnowledgeModelCompleted model result =
    case result of
        Ok km ->
            ( model, cmdNavigate KnowledgeModels )

        Err error ->
            ( { model | deletingKnowledgeModel = Error "Knowledge model could not be deleted" }
            , Cmd.none
            )


handlePostMigration : Session -> Model -> String -> ( Model, Cmd Msgs.Msg )
handlePostMigration session model uuid =
    case model.creatingMigration of
        Loading ->
            ( model, Cmd.none )

        _ ->
            ( { model | creatingMigration = Loading, migrationUuid = Just uuid }, postMigrationCmd uuid session )


postMigrationCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
postMigrationCompleted model result =
    case result of
        Ok migration ->
            ( model, cmdNavigate <| KnowledgeModelsMigration <| Maybe.withDefault "" model.migrationUuid )

        Err error ->
            ( { model | creatingMigration = Error "Migration could not be created" }, Cmd.none )


update : Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg session model =
    case msg of
        GetKnowledgeModelsCompleted result ->
            getKnowledgeModelsCompleted model result

        ShowHideDeleteKnowledgeModel km ->
            ( { model | kmToBeDeleted = km, deletingKnowledgeModel = Unset }, Cmd.none )

        DeleteKnowledgeModel ->
            handleDeleteKM session model

        DeleteKnowledgeModelCompleted result ->
            deleteKnowledgeModelCompleted model result

        PostMigration uuid ->
            handlePostMigration session model uuid

        PostMigrationCompleted result ->
            postMigrationCompleted model result
