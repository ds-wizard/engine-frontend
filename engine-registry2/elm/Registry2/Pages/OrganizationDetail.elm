module Registry2.Pages.OrganizationDetail exposing
    ( Model
    , Msg
    , init
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import Registry2.Api.Models.Organization exposing (Organization)
import Registry2.Api.Organizations as OrganizationsApi
import Registry2.Components.ActionButton as ActionButton
import Registry2.Components.FormGroup as FormGroup
import Registry2.Components.FormResult as FormResult
import Registry2.Components.FormWrapper as FormWrapper
import Registry2.Components.Page as Page
import Registry2.Data.AppState as AppState exposing (AppState)
import Registry2.Data.Forms.OrganizationForm as OrganizationForm exposing (OrganizationForm)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form as Form
import Shared.Form.FormError exposing (FormError)


type alias Model =
    { organization : ActionResult Organization
    , form : Form FormError OrganizationForm
    , saving : ActionResult String
    }


initialModel : Model
initialModel =
    { organization = ActionResult.Loading
    , form = OrganizationForm.init
    , saving = ActionResult.Unset
    }


init : AppState -> ( Model, Cmd Msg )
init appState =
    let
        cmd =
            case AppState.getOrganizationId appState of
                Just organizationId ->
                    OrganizationsApi.getOrganization appState organizationId GetOrganizationCompleted

                Nothing ->
                    Cmd.none
    in
    ( initialModel
    , cmd
    )


setOrganization : ActionResult Organization -> Model -> Model
setOrganization organization model =
    let
        form =
            case organization of
                ActionResult.Success data ->
                    OrganizationForm.initFromOrganization data

                _ ->
                    model.form
    in
    { model
        | organization = organization
        , form = form
    }


type Msg
    = FormMsg Form.Msg
    | GetOrganizationCompleted (Result ApiError Organization)
    | PutOrganizationCompleted (Result ApiError Organization)


update : Msg -> AppState -> Model -> ( Model, Cmd Msg )
update msg appState model =
    case msg of
        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just form ) ->
                    ( { model | saving = ActionResult.Loading }
                    , OrganizationsApi.putOrganization appState
                        (Maybe.withDefault "" (AppState.getOrganizationId appState))
                        form
                        PutOrganizationCompleted
                    )

                _ ->
                    ( { model | form = Form.update OrganizationForm.validation formMsg model.form }
                    , Cmd.none
                    )

        GetOrganizationCompleted result ->
            ( ActionResult.apply setOrganization (ApiError.toActionResult appState (gettext "Unable to get organization detail." appState.locale)) result model
            , Cmd.none
            )

        PutOrganizationCompleted result ->
            let
                newModel =
                    case result of
                        Ok organization ->
                            { model
                                | saving = ActionResult.Success <| gettext "Your changes have been saved." appState.locale
                                , form = OrganizationForm.initFromOrganization organization
                            }

                        Err err ->
                            { model
                                | saving = ApiError.toActionResult appState (gettext "Unable to save changes." appState.locale) err
                                , form = Form.setFormErrors appState err model.form
                            }
            in
            ( newModel, Cmd.none )


view : AppState -> Model -> Html Msg
view appState model =
    Page.view appState (viewOrganizationForm appState model) model.organization


viewOrganizationForm : AppState -> Model -> Organization -> Html Msg
viewOrganizationForm appState model _ =
    FormWrapper.view
        { title = gettext "Edit Organization" appState.locale
        , submitMsg = FormMsg Form.Submit
        , content =
            [ FormResult.view model.saving
            , Html.map FormMsg <| FormGroup.input appState model.form "name" <| gettext "Organization Name" appState.locale
            , Html.map FormMsg <| FormGroup.textarea appState model.form "description" <| gettext "Organization Description" appState.locale
            , Html.map FormMsg <| FormGroup.input appState model.form "email" <| gettext "Email" appState.locale
            , ActionButton.view
                { label = gettext "Save" appState.locale
                , actionResult = model.saving
                }
            ]
        }
