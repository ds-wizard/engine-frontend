module KMEditor.Publish.Update exposing (fetchData, update)

import Auth.Models exposing (Session)
import Common.Models exposing (getServerErrorJwt)
import Common.Types exposing (ActionResult(..))
import Form
import Jwt
import KMEditor.Common.Models exposing (KnowledgeModel)
import KMEditor.Publish.Models exposing (KnowledgeModelPublishForm, Model, encodeKnowledgeModelPublishForm, knowledgeModelPublishFormValidation)
import KMEditor.Publish.Msgs exposing (Msg(..))
import KMEditor.Requests exposing (getKnowledgeModel, putKnowledgeModelVersion)
import KMPackages.Routing
import Msgs
import Requests exposing (getResultCmd)
import Routing exposing (Route(..), cmdNavigate)


fetchData : (Msg -> Msgs.Msg) -> String -> Session -> Cmd Msgs.Msg
fetchData wrapMsg uuid session =
    getKnowledgeModel uuid session
        |> Jwt.send GetKnowledgeModelCompleted
        |> Cmd.map wrapMsg


update : Msg -> (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg session model =
    case msg of
        GetKnowledgeModelCompleted result ->
            getKnowledgeModelCompleted model result

        FormMsg msg ->
            handleForm msg wrapMsg session model

        PutKnowledgeModelVersionCompleted result ->
            putKnowledgeModelVersionCompleted model result


getKnowledgeModelCompleted : Model -> Result Jwt.JwtError KnowledgeModel -> ( Model, Cmd Msgs.Msg )
getKnowledgeModelCompleted model result =
    let
        newModel =
            case result of
                Ok knowledgeModel ->
                    { model | knowledgeModel = Success knowledgeModel }

                Err error ->
                    { model | knowledgeModel = getServerErrorJwt error "Unable to get the knowledge model." }

        cmd =
            getResultCmd result
    in
    ( newModel, Cmd.none )


handleForm : Form.Msg -> (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handleForm formMsg wrapMsg session model =
    case ( formMsg, Form.getOutput model.form, model.knowledgeModel ) of
        ( Form.Submit, Just form, Success km ) ->
            let
                cmd =
                    putKnowledgeModelVersionCmd wrapMsg session form km.uuid
            in
            ( { model | publishingKnowledgeModel = Loading }, cmd )

        _ ->
            let
                form =
                    Form.update knowledgeModelPublishFormValidation formMsg model.form
            in
            ( { model | form = form }, Cmd.none )


putKnowledgeModelVersionCmd : (Msg -> Msgs.Msg) -> Session -> KnowledgeModelPublishForm -> String -> Cmd Msgs.Msg
putKnowledgeModelVersionCmd wrapMsg session form uuid =
    let
        ( version, data ) =
            encodeKnowledgeModelPublishForm form
    in
    putKnowledgeModelVersion uuid version data session
        |> Jwt.send PutKnowledgeModelVersionCompleted
        |> Cmd.map wrapMsg


putKnowledgeModelVersionCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
putKnowledgeModelVersionCompleted model result =
    case result of
        Ok version ->
            ( model, cmdNavigate (KMPackages KMPackages.Routing.Index) )

        Err error ->
            ( { model | publishingKnowledgeModel = getServerErrorJwt error "Publishing new version failed" }
            , getResultCmd result
            )
