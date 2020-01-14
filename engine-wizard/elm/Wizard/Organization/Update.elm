module Wizard.Organization.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.Api.Organizations as OrganizationsApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Organization.Common.Organization exposing (Organization)
import Wizard.Organization.Common.OrganizationForm as OrganizationForm
import Wizard.Organization.Models exposing (..)
import Wizard.Organization.Msgs exposing (Msg(..))


fetchData : AppState -> Cmd Msg
fetchData appState =
    OrganizationsApi.getCurrentOrganization appState GetCurrentOrganizationCompleted


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetCurrentOrganizationCompleted result ->
            getCurrentOrganizationCompleted appState model result

        PutCurrentOrganizationCompleted result ->
            putCurrentOrganizationCompleted appState model result

        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState model


getCurrentOrganizationCompleted : AppState -> Model -> Result ApiError Organization -> ( Model, Cmd Wizard.Msgs.Msg )
getCurrentOrganizationCompleted appState model result =
    let
        newModel =
            case result of
                Ok organization ->
                    { model | form = OrganizationForm.init organization, organization = Success organization }

                Err error ->
                    { model | organization = ApiError.toActionResult (lg "apiError.organizations.getError" appState) error }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


putCurrentOrganizationCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
putCurrentOrganizationCompleted appState model result =
    let
        newResult =
            case result of
                Ok _ ->
                    Success <| lg "apiSuccess.organizations.put" appState

                Err error ->
                    ApiError.toActionResult (lg "apiError.organizations.putError" appState) error

        cmd =
            getResultCmd result
    in
    ( { model | savingOrganization = newResult }, cmd )


handleForm : Form.Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleForm formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.form, model.organization ) of
        ( Form.Submit, Just form, Success organization ) ->
            let
                body =
                    OrganizationForm.encode organization.uuid form

                cmd =
                    Cmd.map wrapMsg <|
                        OrganizationsApi.putCurrentOrganization body appState PutCurrentOrganizationCompleted
            in
            ( { model | savingOrganization = Loading }, cmd )

        _ ->
            let
                form =
                    Form.update OrganizationForm.validation formMsg model.form
            in
            ( { model | form = form }, Cmd.none )
