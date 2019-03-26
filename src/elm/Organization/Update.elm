module Organization.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (getResultCmd)
import Common.Api.Organizations as OrganizationsApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Form exposing (Form)
import Msgs
import Organization.Models exposing (..)
import Organization.Msgs exposing (Msg(..))


fetchData : AppState -> Cmd Msg
fetchData appState =
    OrganizationsApi.getCurrentOrganization appState GetCurrentOrganizationCompleted


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetCurrentOrganizationCompleted result ->
            getCurrentOrganizationCompleted model result

        PutCurrentOrganizationCompleted result ->
            putCurrentOrganizationCompleted model result

        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState model


getCurrentOrganizationCompleted : Model -> Result ApiError Organization -> ( Model, Cmd Msgs.Msg )
getCurrentOrganizationCompleted model result =
    let
        newModel =
            case result of
                Ok organization ->
                    { model | form = initOrganizationForm organization, organization = Success organization }

                Err error ->
                    { model | organization = getServerError error "Unable to get organization information." }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


putCurrentOrganizationCompleted : Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
putCurrentOrganizationCompleted model result =
    let
        newResult =
            case result of
                Ok _ ->
                    Success "Organization was successfuly saved"

                Err error ->
                    getServerError error "Organization could not be saved"

        cmd =
            getResultCmd result
    in
    ( { model | savingOrganization = newResult }, cmd )


handleForm : Form.Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleForm formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.form, model.organization ) of
        ( Form.Submit, Just form, Success organization ) ->
            let
                body =
                    encodeOrganizationForm organization.uuid form

                cmd =
                    Cmd.map wrapMsg <|
                        OrganizationsApi.putCurrentOrganization body appState PutCurrentOrganizationCompleted
            in
            ( { model | savingOrganization = Loading }, cmd )

        _ ->
            let
                form =
                    Form.update organizationFormValidation formMsg model.form
            in
            ( { model | form = form }, Cmd.none )
