module KMEditor.Publish.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (getResultCmd)
import Common.Api.KnowledgeModels as KnowledgeModelsApi
import Common.Api.Packages as PackagesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Form
import Form.Field as Field
import KMEditor.Publish.Models exposing (Model, PublishForm, encodePublishForm, publishFormValidation)
import KMEditor.Publish.Msgs exposing (Msg(..))
import KnowledgeModels.Common.Version as Version exposing (Version)
import KnowledgeModels.Routing
import Msgs
import Routing exposing (Route(..), cmdNavigate)


fetchData : String -> AppState -> Cmd Msg
fetchData uuid appState =
    KnowledgeModelsApi.getKnowledgeModel uuid appState GetKnowledgeModelCompleted


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetKnowledgeModelCompleted result ->
            case result of
                Ok knowledgeModel ->
                    let
                        cmd =
                            case knowledgeModel.parentPackageId of
                                Just parentPackageId ->
                                    Cmd.map wrapMsg <|
                                        PackagesApi.getPackage parentPackageId appState GetParentPackageCompleted

                                Nothing ->
                                    Cmd.none
                    in
                    ( { model | knowledgeModel = Success knowledgeModel }
                    , cmd
                    )

                Err error ->
                    ( { model | knowledgeModel = getServerError error "Unable to get the knowledge model." }
                    , getResultCmd result
                    )

        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState model

        FormSetVersion version ->
            handleFormSetVersion version model

        PutKnowledgeModelVersionCompleted result ->
            putKnowledgeModelVersionCompleted appState model result

        GetParentPackageCompleted result ->
            case result of
                Ok package ->
                    let
                        formMsg field value =
                            Form.Input field Form.Text <| Field.String value

                        form =
                            model.form
                                |> Form.update publishFormValidation (formMsg "description" package.description)
                                |> Form.update publishFormValidation (formMsg "readme" package.readme)
                                |> Form.update publishFormValidation (formMsg "license" package.license)
                    in
                    ( { model | form = form }
                    , Cmd.none
                    )

                Err error ->
                    ( model, getResultCmd result )


handleForm : Form.Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleForm formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.form, model.knowledgeModel ) of
        ( Form.Submit, Just form, Success km ) ->
            let
                ( version, body ) =
                    encodePublishForm form

                cmd =
                    Cmd.map wrapMsg <|
                        KnowledgeModelsApi.putVersion km.uuid version body appState PutKnowledgeModelVersionCompleted
            in
            ( { model | publishingKnowledgeModel = Loading }, cmd )

        _ ->
            let
                form =
                    Form.update publishFormValidation formMsg model.form
            in
            ( { model | form = form }, Cmd.none )


handleFormSetVersion : Version -> Model -> ( Model, Cmd Msgs.Msg )
handleFormSetVersion version model =
    let
        formMsg field value =
            Form.Input field Form.Text <| Field.String (String.fromInt value)

        form =
            model.form
                |> Form.update publishFormValidation (formMsg "major" <| Version.getMajor version)
                |> Form.update publishFormValidation (formMsg "minor" <| Version.getMinor version)
                |> Form.update publishFormValidation (formMsg "patch" <| Version.getPatch version)
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
