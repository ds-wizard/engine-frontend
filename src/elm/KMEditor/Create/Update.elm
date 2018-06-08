module KMEditor.Create.Update exposing (fetchData, update)

import Auth.Models exposing (Session)
import Common.Form exposing (setFormErrorsJwt)
import Common.Models exposing (getServerErrorJwt)
import Common.Types exposing (ActionResult(..))
import Form exposing (Form)
import Jwt
import KMEditor.Create.Models exposing (..)
import KMEditor.Create.Msgs exposing (Msg(..))
import KMEditor.Requests exposing (postKnowledgeModel)
import KMEditor.Routing exposing (Route(Editor, Index))
import KMPackages.Common.Models exposing (PackageDetail)
import KMPackages.Requests exposing (getPackages)
import Msgs
import Random.Pcg exposing (Seed)
import Requests exposing (getResultCmd)
import Routing exposing (Route(..), cmdNavigate)
import Utils exposing (getUuid, tuplePrepend)


fetchData : (Msg -> Msgs.Msg) -> Session -> Cmd Msgs.Msg
fetchData wrapMsg session =
    getPackages session
        |> Jwt.send GetPackagesCompleted
        |> Cmd.map wrapMsg


update : Msg -> (Msg -> Msgs.Msg) -> Seed -> Session -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg wrapMsg seed session model =
    case msg of
        GetPackagesCompleted result ->
            getPackageCompleted model result |> tuplePrepend seed

        FormMsg formMsg ->
            handleForm formMsg wrapMsg seed session model

        PostKnowledgeModelCompleted result ->
            postKmCompleted model result |> tuplePrepend seed


getPackageCompleted : Model -> Result Jwt.JwtError (List PackageDetail) -> ( Model, Cmd Msgs.Msg )
getPackageCompleted model result =
    let
        newModel =
            case result of
                Ok packages ->
                    setSelectedPackage { model | packages = Success packages } packages

                Err error ->
                    { model | packages = getServerErrorJwt error "Unable to get package list" }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


setSelectedPackage : Model -> List PackageDetail -> Model
setSelectedPackage model packages =
    case model.selectedPackage of
        Just id ->
            if List.any (.id >> (==) id) packages then
                { model | form = initKnowledgeModelCreateForm model.selectedPackage }
            else
                model

        _ ->
            model


handleForm : Form.Msg -> (Msg -> Msgs.Msg) -> Seed -> Session -> Model -> ( Seed, Model, Cmd Msgs.Msg )
handleForm formMsg wrapMsg seed session model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just kmCreateForm ) ->
            let
                ( newUuid, newSeed ) =
                    getUuid seed

                cmd =
                    postKmCmd wrapMsg session kmCreateForm newUuid
            in
            ( newSeed, { model | savingKnowledgeModel = Loading, newUuid = Just newUuid }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update knowledgeModelCreateFormValidation formMsg model.form }
            in
            ( seed, newModel, Cmd.none )


postKmCmd : (Msg -> Msgs.Msg) -> Session -> KnowledgeModelCreateForm -> String -> Cmd Msgs.Msg
postKmCmd wrapMsg session form uuid =
    form
        |> encodeKnowledgeCreateModelForm uuid
        |> postKnowledgeModel session
        |> Jwt.send PostKnowledgeModelCompleted
        |> Cmd.map wrapMsg


postKmCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
postKmCompleted model result =
    case result of
        Ok km ->
            ( model
            , Maybe.map (Routing.KMEditor << Editor) model.newUuid
                |> Maybe.withDefault (Routing.KMEditor Index)
                |> cmdNavigate
            )

        Err error ->
            ( { model
                | form = setFormErrorsJwt error model.form
                , savingKnowledgeModel = getServerErrorJwt error "Knowledge model could not be created."
              }
            , getResultCmd result
            )
