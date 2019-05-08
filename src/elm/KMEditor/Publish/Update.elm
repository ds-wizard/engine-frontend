module KMEditor.Publish.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (applyResult, getResultCmd)
import Common.Api.KnowledgeModels as KnowledgeModelsApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Common.Setters exposing (setKnowledgeModel)
import Form
import KMEditor.Publish.Models exposing (KnowledgeModelPublishForm, Model, encodeKnowledgeModelPublishForm, knowledgeModelPublishFormValidation)
import KMEditor.Publish.Msgs exposing (Msg(..))
import KnowledgeModels.Routing
import Msgs
import Routing exposing (Route(..), cmdNavigate)


fetchData : (Msg -> Msgs.Msg) -> String -> AppState -> Cmd Msgs.Msg
fetchData wrapMsg uuid appState =
    Cmd.map wrapMsg <|
        KnowledgeModelsApi.getKnowledgeModel uuid appState GetKnowledgeModelCompleted


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetKnowledgeModelCompleted result ->
            applyResult
                { setResult = setKnowledgeModel
                , defaultError = "Unable to get the knowledge model."
                , model = model
                , result = result
                }

        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState model

        PutKnowledgeModelVersionCompleted result ->
            putKnowledgeModelVersionCompleted appState model result


handleForm : Form.Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleForm formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.form, model.knowledgeModel ) of
        ( Form.Submit, Just form, Success km ) ->
            let
                ( version, body ) =
                    encodeKnowledgeModelPublishForm form

                cmd =
                    Cmd.map wrapMsg <|
                        KnowledgeModelsApi.putVersion km.uuid version body appState PutKnowledgeModelVersionCompleted
            in
            ( { model | publishingKnowledgeModel = Loading }, cmd )

        _ ->
            let
                form =
                    Form.update knowledgeModelPublishFormValidation formMsg model.form
            in
            ( { model | form = form }, Cmd.none )


putKnowledgeModelVersionCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
putKnowledgeModelVersionCompleted appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState.key (KnowledgeModels KnowledgeModels.Routing.Index) )

        Err error ->
            ( { model | publishingKnowledgeModel = getServerError error "Publishing new version failed" }
            , getResultCmd result
            )
