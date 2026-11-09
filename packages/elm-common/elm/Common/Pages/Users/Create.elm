module Common.Pages.Users.Create exposing
    ( Model
    , Msg
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Api.Models.Role as Role exposing (Role)
import Common.Api.Models.Settings exposing (Settings)
import Common.Api.Models.Settings.RolesSettings as RolesSettings exposing (RolesSettings)
import Common.Api.Roles as RolesApi
import Common.Api.Settings as SettingsApi
import Common.Api.Users as UsersApi
import Common.Components.Form as CommonForm
import Common.Components.Form.Field as Field
import Common.Components.GuideLink as GuideLink
import Common.Components.Page as Page
import Common.Components.PasswordStrength as PasswordStrength
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.Forms.UserCreateForm as UserCreateForm exposing (UserCreateForm)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Ports.Window as Window
import Common.Routes as Routes
import Common.Utils.Form as FormUtils
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.RequestHelpers as RequestHelpers
import Form exposing (Form)
import Form.Field as FormField
import Gettext exposing (gettext)
import Html exposing (Html)
import Random exposing (Seed)
import Uuid



-- MODEL


type alias Model =
    { roles : ActionResult (List Role)
    , savingUser : ActionResult String
    , form : Form FormError UserCreateForm
    , passwordStrength : PasswordStrength.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { roles = Loading
    , savingUser = Unset
    , form = UserCreateForm.init appState
    , passwordStrength = PasswordStrength.init appState
    }


init : AppState -> ( Model, Cmd Msg )
init appState =
    ( initialModel appState
    , Cmd.batch
        [ RolesApi.getRoles appState GetRolesCompleted
        , SettingsApi.get appState RolesSettings.section GetDefaultRoleCompleted
        ]
    )



-- UPDATE


type Msg
    = GetRolesCompleted (Result ApiError (Pagination Role))
    | GetDefaultRoleCompleted (Result ApiError (Settings RolesSettings))
    | Cancel
    | FormMsg Form.Msg
    | PostUserCompleted (Result ApiError ())
    | PasswordStrengthMsg PasswordStrength.Msg


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Seed, Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetRolesCompleted result ->
            case result of
                Ok pagination ->
                    ( appState.seed, { model | roles = Success pagination.items }, Cmd.none )

                Err error ->
                    ( appState.seed
                    , { model | roles = ApiError.toActionResult appState (gettext "Unable to get the roles." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        -- The role new people get, chosen for a start unless a role has been
        -- chosen already. Without it the choice is simply left to be made.
        GetDefaultRoleCompleted result ->
            case ( result, (Form.getFieldAsString "roleUuid" model.form).value ) of
                ( Ok settings, Nothing ) ->
                    ( appState.seed
                    , { model
                        | form =
                            Form.update (PasswordStrength.validation appState (passwordField appState) model.passwordStrength)
                                (Form.Input "roleUuid" Form.Select (FormField.String (Uuid.toString settings.value.defaultRoleUuid)))
                                model.form
                      }
                    , Cmd.none
                    )

                _ ->
                    ( appState.seed, model, Cmd.none )

        Cancel ->
            ( appState.seed, model, Window.historyBack (Routes.toUrl appState Routes.usersIndex) )

        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form, PasswordStrength.isChecking model.passwordStrength ) of
                ( Form.Submit, Just userCreateForm, False ) ->
                    let
                        ( newUuid, newSeed ) =
                            Random.step Uuid.uuidGenerator appState.seed

                        body =
                            UserCreateForm.encode (Uuid.toString newUuid) userCreateForm
                    in
                    ( newSeed
                    , { model | savingUser = Loading }
                    , Cmd.map cfg.wrapMsg (UsersApi.postUser appState body PostUserCompleted)
                    )

                _ ->
                    let
                        updated =
                            PasswordStrength.updateForm appState (passwordField appState) formMsg model.form model.passwordStrength
                    in
                    ( appState.seed
                    , { model | form = updated.form, passwordStrength = updated.passwordStrength }
                    , PasswordStrength.toCmd (cfg.wrapMsg << PasswordStrengthMsg) (cfg.wrapMsg (FormMsg Form.Submit)) updated
                    )

        PasswordStrengthMsg passwordStrengthMsg ->
            let
                updated =
                    PasswordStrength.update appState (passwordField appState) passwordStrengthMsg model.form model.passwordStrength
            in
            ( appState.seed
            , { model | form = updated.form, passwordStrength = updated.passwordStrength }
            , PasswordStrength.toCmd (cfg.wrapMsg << PasswordStrengthMsg) (cfg.wrapMsg (FormMsg Form.Submit)) updated
            )

        PostUserCompleted result ->
            case result of
                Ok _ ->
                    ( appState.seed, model, Routes.navigate appState Routes.usersIndex )

                Err error ->
                    ( appState.seed
                    , { model
                        | savingUser = ApiError.toActionResult appState (gettext "User could not be created." appState.locale) error
                        , form = FormUtils.setFormErrors appState error model.form
                      }
                    , Cmd.batch
                        [ RequestHelpers.getResultCmd cfg.logoutMsg result
                        , CommonForm.focusErrorSummary
                        ]
                    )


passwordField : AppState -> PasswordStrength.PasswordField UserCreateForm
passwordField appState =
    { path = "password", validation = UserCreateForm.validation appState }



-- SUBSCRIPTIONS


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    if ActionResult.isSuccess model.roles then
        CommonForm.submitShortcut appState { result = model.savingUser, onSubmit = FormMsg Form.Submit }

    else
        Sub.none



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResult appState (viewContent appState model) model.roles


viewContent : AppState -> Model -> List Role -> Html Msg
viewContent appState model roles =
    CommonForm.createPage appState
        { title = gettext "Create User" appState.locale
        , description = Nothing
        , actions = [ GuideLink.button appState (AppState.toServerInfo appState) GuideLinks.usersCreate ]
        , content = List.map (Html.map FormMsg) (formFields appState roles model.form)
        , submitLabel = gettext "Create" appState.locale
        , result = model.savingUser
        , onSubmit = FormMsg Form.Submit
        , onCancel = Just Cancel
        }


formFields : AppState -> List Role -> Form FormError UserCreateForm -> List (Html Form.Msg)
formFields appState roles form =
    [ Field.email appState form "email" (gettext "Email" appState.locale)
    , Field.input appState form "firstName" (gettext "First name" appState.locale)
    , Field.input appState form "lastName" (gettext "Last name" appState.locale)
    , Field.input appState form "affiliation" (gettext "Affiliation" appState.locale)
        |> Field.withSuggestions appState.config.users.affiliations
    , Field.select appState form "roleUuid" (gettext "Role" appState.locale) (Role.toFormOptions appState.locale roles)
    , Field.secret appState form "password" (gettext "Password" appState.locale)
        |> Field.withAutocomplete "new-password"
    ]
        |> List.map Field.toHtml
