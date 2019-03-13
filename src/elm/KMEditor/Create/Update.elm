module KMEditor.Create.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Auth.Models exposing (Session)
import Common.Form exposing (setFormErrorsJwt)
import Common.Models exposing (getServerErrorJwt)
import Form exposing (Form)
import Jwt
import KMEditor.Common.Models exposing (KnowledgeModel)
import KMEditor.Create.Models exposing (..)
import KMEditor.Create.Msgs exposing (Msg(..))
import KMEditor.Requests exposing (postKnowledgeModel)
import KMEditor.Routing exposing (Route(..))
import KnowledgeModels.Common.Models exposing (PackageDetail)
import KnowledgeModels.Requests exposing (getPackages)
import Models exposing (State)
import Msgs
import Requests exposing (getResultCmd)
import Routing exposing (Route(..), cmdNavigate)
import Utils exposing (getUuid, tuplePrepend)


fetchData : (Msg -> Msgs.Msg) -> Session -> Cmd Msgs.Msg
fetchData wrapMsg session =
    getPackages session
        |> Jwt.send GetPackagesCompleted
        |> Cmd.map wrapMsg


update : Msg -> (Msg -> Msgs.Msg) -> State -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg state model =
    case msg of
        GetPackagesCompleted result ->
            getPackageCompleted model result

        FormMsg formMsg ->
            handleForm formMsg wrapMsg state.session model

        PostKnowledgeModelCompleted result ->
            postKmCompleted state model result


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


handleForm : Form.Msg -> (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handleForm formMsg wrapMsg session model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just kmCreateForm ) ->
            let
                cmd =
                    postKmCmd wrapMsg session kmCreateForm
            in
            ( { model | savingKnowledgeModel = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update knowledgeModelCreateFormValidation formMsg model.form }
            in
            ( newModel, Cmd.none )


postKmCmd : (Msg -> Msgs.Msg) -> Session -> KnowledgeModelCreateForm -> Cmd Msgs.Msg
postKmCmd wrapMsg session form =
    form
        |> encodeKnowledgeCreateModelForm
        |> postKnowledgeModel session
        |> Jwt.send PostKnowledgeModelCompleted
        |> Cmd.map wrapMsg


postKmCompleted : State -> Model -> Result Jwt.JwtError KnowledgeModel -> ( Model, Cmd Msgs.Msg )
postKmCompleted state model result =
    case result of
        Ok km ->
            ( model
            , cmdNavigate state.key (Routing.KMEditor <| EditorRoute km.uuid)
            )

        Err error ->
            ( { model
                | form = setFormErrorsJwt error model.form
                , savingKnowledgeModel = getServerErrorJwt error "Knowledge model could not be created."
              }
            , getResultCmd result
            )
