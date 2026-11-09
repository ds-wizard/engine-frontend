module Common.Pages.Users.Edit.Password exposing
    ( Model
    , Msg
    , footer
    , initialModel
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Users as UsersApi
import Common.Components.Form as CommonForm
import Common.Components.Form.Field as Field
import Common.Components.PasswordStrength as PasswordStrength
import Common.Data.AppState exposing (AppState)
import Common.Data.Forms.UserPasswordForm as UserPasswordForm exposing (UserPasswordForm)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Data.UuidOrCurrent exposing (UuidOrCurrent)
import Common.Pages.Users.Components.Section as Section
import Common.Utils.Form as FormUtils
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.RequestHelpers as RequestHelpers
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.Alert as Alert



-- MODEL


type alias Model =
    { uuidOrCurrent : UuidOrCurrent
    , passwordForm : Form FormError UserPasswordForm
    , savingPassword : ActionResult String
    , passwordStrength : PasswordStrength.Model
    }


initialModel : AppState -> UuidOrCurrent -> Model
initialModel appState uuidOrCurrent =
    { uuidOrCurrent = uuidOrCurrent
    , passwordForm = UserPasswordForm.init appState
    , savingPassword = Unset
    , passwordStrength = PasswordStrength.init appState
    }



-- UPDATE


type Msg
    = PasswordFormMsg Form.Msg
    | PutUserPasswordCompleted (Result ApiError ())
    | PasswordStrengthMsg PasswordStrength.Msg
    | Discard
    | NoOp


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        PasswordFormMsg formMsg ->
            case ( formMsg, Form.getOutput model.passwordForm, PasswordStrength.isChecking model.passwordStrength ) of
                ( Form.Submit, Just passwordForm, False ) ->
                    ( { model | savingPassword = Loading }
                    , Cmd.map cfg.wrapMsg <|
                        UsersApi.putUserPassword appState model.uuidOrCurrent (UserPasswordForm.encode passwordForm) PutUserPasswordCompleted
                    )

                _ ->
                    let
                        updated =
                            PasswordStrength.updateForm appState (passwordField appState) formMsg model.passwordForm model.passwordStrength
                    in
                    ( { model | passwordForm = updated.form, passwordStrength = updated.passwordStrength }
                    , PasswordStrength.toCmd (cfg.wrapMsg << PasswordStrengthMsg) (cfg.wrapMsg (PasswordFormMsg Form.Submit)) updated
                    )

        PasswordStrengthMsg passwordStrengthMsg ->
            let
                updated =
                    PasswordStrength.update appState (passwordField appState) passwordStrengthMsg model.passwordForm model.passwordStrength
            in
            ( { model | passwordForm = updated.form, passwordStrength = updated.passwordStrength }
            , PasswordStrength.toCmd (cfg.wrapMsg << PasswordStrengthMsg) (cfg.wrapMsg (PasswordFormMsg Form.Submit)) updated
            )

        Discard ->
            -- The bar's buttons go with the bar, so focus would be left on
            -- nothing: it goes to the page's heading, as it does in settings.
            ( { model
                | passwordForm = UserPasswordForm.init appState
                , savingPassword = Unset
                , passwordStrength = PasswordStrength.init appState
              }
            , Cmd.map cfg.wrapMsg (Section.focusHeading NoOp)
            )

        NoOp ->
            ( model, Cmd.none )

        PutUserPasswordCompleted result ->
            case result of
                Ok _ ->
                    -- Emptied, so the bar that offers to save it goes with the
                    -- password it was offering to save.
                    ( { model
                        | savingPassword = Success (gettext "Password was successfully changed." appState.locale)
                        , passwordForm = UserPasswordForm.init appState
                        , passwordStrength = PasswordStrength.init appState
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | savingPassword = ApiError.toActionResult appState (gettext "Password could not be changed." appState.locale) error }
                    , Cmd.batch
                        [ RequestHelpers.getResultCmd cfg.logoutMsg result
                        , CommonForm.focusErrorSummary
                        ]
                    )


passwordField : AppState -> PasswordStrength.PasswordField UserPasswordForm
passwordField appState =
    { path = "password", validation = UserPasswordForm.validation appState }



-- SUBSCRIPTIONS


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    CommonForm.submitShortcut appState { result = model.savingPassword, onSubmit = PasswordFormMsg Form.Submit }



-- VIEW


view : AppState -> Model -> List (Html Msg)
view appState model =
    let
        successAlert =
            case model.savingPassword of
                Success message ->
                    Alert.toHtml (Alert.success message)

                _ ->
                    Html.text ""

        fields =
            [ Field.passwordWithStrength appState model.passwordStrength model.passwordForm "password" (gettext "New password" appState.locale)
                |> Field.withAutocomplete "new-password"
            , Field.password appState model.passwordForm "passwordConfirmation" (gettext "New password again" appState.locale)
                |> Field.withAutocomplete "new-password"
            ]
    in
    Section.view appState
        { title = gettext "Password" appState.locale
        , guideLink = Just GuideLinks.profilePassword
        , content =
            [ CommonForm.view
                { onSubmit = PasswordFormMsg Form.Submit
                , content =
                    successAlert
                        :: CommonForm.errorSummary model.savingPassword
                        :: List.map (Html.map PasswordFormMsg << Field.toHtml) fields
                }
            ]
        }


{-| The unsaved-changes bar, in the settings template's footer slot: the same
save the rest of the profile has, which appears once there is a password to
set.
-}
footer : AppState -> Model -> Maybe (Html Msg)
footer appState model =
    Just
        (CommonForm.unsavedBar appState
            { changed = FormUtils.containsChanges model.passwordForm
            , result = model.savingPassword
            , onSave = PasswordFormMsg Form.Submit
            , onDiscard = Just Discard
            }
        )
