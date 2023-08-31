module Wizard.Users.Edit.Components.Profile exposing
    ( Model
    , Msg(..)
    , UpdateConfig
    , fetchData
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, div, img, strong, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onSubmit)
import Shared.Api.Users as UsersApi
import Shared.Auth.Role as Role
import Shared.Common.UuidOrCurrent as UuidOrCurrent exposing (UuidOrCurrent)
import Shared.Data.User as User exposing (User)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form as Form
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode)
import Shared.Markdown as Markdown
import Shared.Utils exposing (dispatch)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (wideDetailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.ExternalLoginButton as ExternalLoginButton
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Ports as Ports
import Wizard.Users.Common.UserEditForm as UserEditForm exposing (UserEditForm)


type alias Model =
    { uuidOrCurrent : UuidOrCurrent
    , user : ActionResult User
    , savingUser : ActionResult String
    , userForm : Form FormError UserEditForm
    }


initialModel : UuidOrCurrent -> Model
initialModel uuidOrCurrent =
    { uuidOrCurrent = uuidOrCurrent
    , user = ActionResult.Loading
    , savingUser = ActionResult.Unset
    , userForm = UserEditForm.initEmpty
    }


type Msg
    = GetUserCompleted (Result ApiError User)
    | EditFormMsg Form.Msg
    | PutUserCompleted (Result ApiError User)


fetchData : AppState -> UuidOrCurrent -> Cmd Msg
fetchData appState uuidOrCurrent =
    UsersApi.getUser uuidOrCurrent appState GetUserCompleted


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    , updateUserMsg : User -> msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        EditFormMsg formMsg ->
            handleUserForm cfg appState formMsg model

        GetUserCompleted result ->
            getUserCompleted cfg appState model result

        PutUserCompleted result ->
            putUserCompleted cfg appState model result


handleUserForm : UpdateConfig msg -> AppState -> Form.Msg -> Model -> ( Model, Cmd msg )
handleUserForm cfg appState formMsg model =
    case ( formMsg, Form.getOutput model.userForm ) of
        ( Form.Submit, Just userForm ) ->
            let
                body =
                    UserEditForm.encode model.uuidOrCurrent userForm

                cmd =
                    Cmd.map cfg.wrapMsg <|
                        UsersApi.putUser model.uuidOrCurrent body appState PutUserCompleted
            in
            ( { model | savingUser = ActionResult.Loading }, cmd )

        _ ->
            let
                userForm =
                    Form.update UserEditForm.validation formMsg model.userForm
            in
            ( { model | userForm = userForm }, Cmd.none )


getUserCompleted : UpdateConfig msg -> AppState -> Model -> Result ApiError User -> ( Model, Cmd msg )
getUserCompleted cfg appState model result =
    let
        newModel =
            case result of
                Ok user ->
                    let
                        userForm =
                            UserEditForm.init user
                    in
                    { model | userForm = userForm, user = ActionResult.Success user }

                Err _ ->
                    { model | user = ActionResult.Error <| gettext "Unable to get the user." appState.locale }

        cmd =
            getResultCmd cfg.logoutMsg result
    in
    ( newModel, cmd )


putUserCompleted : UpdateConfig msg -> AppState -> Model -> Result ApiError User -> ( Model, Cmd msg )
putUserCompleted cfg appState model result =
    case result of
        Ok user ->
            let
                updateCmd =
                    if Just user.uuid == Maybe.map .uuid appState.session.user then
                        dispatch (cfg.updateUserMsg user)

                    else
                        Cmd.none
            in
            ( { model | savingUser = ActionResult.Success <| gettext "Profile was successfully updated." appState.locale }
            , Cmd.batch
                [ Ports.scrollToTop ".Users__Edit__content"
                , updateCmd
                ]
            )

        Err err ->
            ( { model
                | savingUser = ApiError.toActionResult appState (gettext "Profile could not be saved." appState.locale) err
                , userForm = Form.setFormErrors appState err model.userForm
              }
            , Cmd.batch
                [ getResultCmd cfg.logoutMsg result
                , Ports.scrollToTop ".Users__Edit__content"
                ]
            )


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (userView appState model) model.user


userView : AppState -> Model -> User -> Html Msg
userView appState model user =
    div [ wideDetailClass "" ]
        [ Page.header (gettext "Profile" appState.locale) []
        , div [ class "row" ]
            [ Html.form [ onSubmit (EditFormMsg Form.Submit), class "col-8" ]
                [ FormResult.view appState model.savingUser
                , Html.map EditFormMsg <| userFormView appState user model.userForm (UuidOrCurrent.isCurrent model.uuidOrCurrent)
                , div [ class "mt-5" ]
                    [ ActionButton.submit appState (ActionButton.SubmitConfig (gettext "Save" appState.locale) model.savingUser) ]
                ]
            , div [ class "col-4" ]
                [ div [ class "col-border-left" ]
                    [ strong [] [ text (gettext "User Image" appState.locale) ]
                    , div []
                        [ img [ src (User.imageUrl user), class "user-icon user-icon-large" ] []
                        ]
                    , Markdown.toHtml [ class "text-muted" ] (gettext "Image is taken from OpenID profile or [Gravatar](https://gravatar.com)." appState.locale)
                    ]
                ]
            ]
        ]


userFormView : AppState -> User -> Form FormError UserEditForm -> Bool -> Html Form.Msg
userFormView appState user form isCurrent =
    let
        roleSelect =
            if isCurrent then
                emptyNode

            else
                FormGroup.select appState (Role.options appState) form "role" <| gettext "Role" appState.locale

        activeToggle =
            if isCurrent then
                emptyNode

            else
                FormGroup.toggle form "active" <| gettext "Active" appState.locale
    in
    div []
        [ FormGroup.input appState form "email" <| gettext "Email" appState.locale
        , FormExtra.blockAfter (List.map (ExternalLoginButton.badgeWrapper appState) user.sources)
        , FormGroup.input appState form "firstName" <| gettext "First name" appState.locale
        , FormGroup.input appState form "lastName" <| gettext "Last name" appState.locale
        , FormGroup.inputWithTypehints appState.config.organization.affiliations appState form "affiliation" <| gettext "Affiliation" appState.locale
        , roleSelect
        , activeToggle
        ]
