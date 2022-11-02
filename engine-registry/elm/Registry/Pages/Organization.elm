module Registry.Pages.Organization exposing
    ( Model
    , Msg
    , OrganizationForm
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Form.Field as Field exposing (Field)
import Form.Validate as Validate exposing (Validation)
import Gettext exposing (gettext)
import Html exposing (Html, div, form, h1, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import Registry.Common.AppState exposing (AppState)
import Registry.Common.Credentials exposing (Credentials)
import Registry.Common.Entities.OrganizationDetail exposing (OrganizationDetail)
import Registry.Common.Requests as Requests
import Registry.Common.View.ActionButton as ActionButton
import Registry.Common.View.FormGroup as FormGroup
import Registry.Common.View.FormResult as FormResult
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form exposing (setFormErrors)
import Shared.Form.FormError exposing (FormError)


init : AppState -> Credentials -> ( Model, Cmd Msg )
init appState credentials =
    ( { organization = Loading
      , form = initOrganizationForm []
      , saving = Unset
      }
    , Requests.getOrganization credentials appState GetOrganizationCompleted
    )



-- MODEL


type alias Model =
    { organization : ActionResult OrganizationDetail
    , form : Form FormError OrganizationForm
    , saving : ActionResult String
    }


type alias OrganizationForm =
    { name : String
    , description : String
    , email : String
    }


setOrganization : ActionResult OrganizationDetail -> Model -> Model
setOrganization organization model =
    let
        form =
            case organization of
                Success data ->
                    initOrganizationForm <| organizationFormInitials data

                _ ->
                    model.form
    in
    { model
        | organization = organization
        , form = form
    }


organizationFormValidation : Validation e OrganizationForm
organizationFormValidation =
    Validate.map3 OrganizationForm
        (Validate.field "name" Validate.string)
        (Validate.field "description" Validate.string)
        (Validate.field "email" Validate.email)


organizationFormInitials : OrganizationDetail -> List ( String, Field )
organizationFormInitials organization =
    [ ( "name", Field.string organization.name )
    , ( "description", Field.string organization.description )
    , ( "email", Field.string organization.email )
    ]


initOrganizationForm : List ( String, Field ) -> Form e OrganizationForm
initOrganizationForm initials =
    Form.initial initials organizationFormValidation



-- UPDATE


type Msg
    = FormMsg Form.Msg
    | GetOrganizationCompleted (Result ApiError OrganizationDetail)
    | PutOrganizationCompleted (Result ApiError OrganizationDetail)


update : Msg -> AppState -> Model -> ( Model, Cmd Msg )
update msg appState model =
    case msg of
        FormMsg formMsg ->
            handleFormMsg formMsg appState model

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
                                | saving = Success <| gettext "Your changes have been saved." appState.locale
                                , form = initOrganizationForm <| organizationFormInitials organization
                            }

                        Err err ->
                            { model
                                | saving = ApiError.toActionResult appState (gettext "Unable to save changes." appState.locale) err
                                , form = setFormErrors appState err model.form
                            }
            in
            ( newModel, Cmd.none )


handleFormMsg : Form.Msg -> AppState -> Model -> ( Model, Cmd Msg )
handleFormMsg formMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just form ) ->
            ( { model | saving = Loading }
            , Requests.putOrganization form appState PutOrganizationCompleted
            )

        _ ->
            ( { model | form = Form.update organizationFormValidation formMsg model.form }
            , Cmd.none
            )



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    div [ class "Organization" ]
        [ h1 [] [ text (gettext "Edit Organization" appState.locale) ]
        , form [ onSubmit <| FormMsg Form.Submit ]
            [ FormResult.view model.saving
            , Html.map FormMsg <| FormGroup.input appState model.form "name" <| gettext "Organization Name" appState.locale
            , Html.map FormMsg <| FormGroup.textarea appState model.form "description" <| gettext "Organization Description" appState.locale
            , Html.map FormMsg <| FormGroup.input appState model.form "email" <| gettext "Email" appState.locale
            , ActionButton.submit ( gettext "Save" appState.locale, model.saving )
            ]
        ]
