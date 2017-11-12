module KnowledgeModels.Create.Update exposing (..)

import Auth.Models exposing (Session)
import Form exposing (Form)
import Jwt
import KnowledgeModels.Create.Models exposing (Model)
import KnowledgeModels.Create.Msgs exposing (Msg(..))
import KnowledgeModels.Models exposing (..)
import KnowledgeModels.Requests exposing (postKnowledgeModel)
import Msgs
import PackageManagement.Models exposing (PackageDetail)
import PackageManagement.Requests exposing (getPackages)
import Random.Pcg exposing (Seed, step)
import Requests exposing (toCmd)
import Routing exposing (Route(..), cmdNavigate)
import Utils exposing (tuplePrepend)
import Uuid


getPackagesCmd : Session -> Cmd Msgs.Msg
getPackagesCmd session =
    getPackages session
        |> toCmd GetPackagesCompleted Msgs.KnowledgeModelsCreateMsg


postKmCmd : Session -> KnowledgeModelCreateForm -> String -> Cmd Msgs.Msg
postKmCmd session form uuid =
    form
        |> encodeKnowledgeModelForm uuid
        |> postKnowledgeModel session
        |> toCmd PostKnowledgeModelCompleted Msgs.KnowledgeModelsCreateMsg


getPackageCompleted : Model -> Result Jwt.JwtError (List PackageDetail) -> ( Model, Cmd Msgs.Msg )
getPackageCompleted model result =
    let
        newModel =
            case result of
                Ok packages ->
                    { model | packages = packages }

                Err error ->
                    { model | loadingError = "Unable to get package list" }
    in
    ( { newModel | loading = False }, Cmd.none )


postKmCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
postKmCompleted model result =
    case result of
        Ok km ->
            ( model, cmdNavigate KnowledgeModels )

        Err error ->
            ( { model | error = "Knowledge model could not be created.", savingKm = False }, Cmd.none )


handleForm : Form.Msg -> Seed -> Session -> Model -> ( Seed, Model, Cmd Msgs.Msg )
handleForm formMsg seed session model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just kmCreateForm ) ->
            let
                ( newUuid, newSeed ) =
                    step Uuid.uuidGenerator seed

                cmd =
                    Uuid.toString newUuid |> postKmCmd session kmCreateForm
            in
            ( newSeed, { model | savingKm = True }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update knowledgeModelCreateFormValidation formMsg model.form }
            in
            ( seed, newModel, Cmd.none )


update : Msg -> Seed -> Session -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg seed session model =
    case msg of
        GetPackagesCompleted result ->
            getPackageCompleted model result |> tuplePrepend seed

        FormMsg formMsg ->
            handleForm formMsg seed session model

        PostKnowledgeModelCompleted result ->
            postKmCompleted model result |> tuplePrepend seed

        _ ->
            ( seed, model, Cmd.none )
