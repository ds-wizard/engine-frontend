module Wizard.Pages.Users.Edit.Components.Profile exposing
    ( Model
    , Msg
    , UpdateConfig
    , fetchData
    , initialModel
    , setUser
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Api.Models.Role as Role exposing (Role)
import Common.Components.ActionButton as ActionButton
import Common.Components.FontAwesome exposing (fa, faInfo)
import Common.Components.FormGroup as FormGroup
import Common.Components.FormResult as FormResult
import Common.Components.Page as Page
import Common.Data.UuidOrCurrent as UuidOrCurrent exposing (UuidOrCurrent)
import Common.Ports.Dom as Dom
import Common.Ports.FormUtils as FormUtils
import Common.Ports.Window as Window
import Common.Utils.Form as Form
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Markdown as Markdown
import Common.Utils.RequestHelpers as RequestHelpers
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, a, div, img, strong, text)
import Html.Attributes exposing (class, href, src)
import Html.Events exposing (onSubmit)
import Html.Extra as Html
import Wizard.Api.Models.BootstrapConfig.AdminConfig as Admin
import Wizard.Api.Models.User as User exposing (User)
import Wizard.Api.Roles as RolesApi
import Wizard.Api.Users as UsersApi
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Users.Common.UserEditForm as UserEditForm exposing (UserEditForm)


type alias Model =
    { uuidOrCurrent : UuidOrCurrent
    , user : ActionResult User
    , roles : ActionResult (List Role)
    , savingUser : ActionResult String
    , userForm : Form FormError UserEditForm
    }


initialModel : UuidOrCurrent -> Model
initialModel uuidOrCurrent =
    { uuidOrCurrent = uuidOrCurrent
    , user = ActionResult.Loading
    , roles = ActionResult.Loading
    , savingUser = ActionResult.Unset
    , userForm = UserEditForm.initEmpty
    }


type Msg
    = GetRolesCompleted (Result ApiError (Pagination Role))
    | EditFormMsg Form.Msg
    | PutUserCompleted (Result ApiError User)


fetchData : AppState -> UuidOrCurrent -> Cmd Msg
fetchData appState uuidOrCurrent =
    if UuidOrCurrent.isCurrent uuidOrCurrent then
        Cmd.none

    else
        RolesApi.getRoles appState GetRolesCompleted


setUser : User -> Model -> Model
setUser user model =
    { model
        | user = ActionResult.Success user
        , userForm = UserEditForm.init user
    }


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        EditFormMsg formMsg ->
            handleUserForm cfg appState formMsg model

        GetRolesCompleted result ->
            getRolesCompleted cfg appState model result

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
                        UsersApi.putUser appState model.uuidOrCurrent body PutUserCompleted
            in
            ( { model | savingUser = ActionResult.Loading }, cmd )

        _ ->
            let
                userForm =
                    Form.update UserEditForm.validation formMsg model.userForm
            in
            ( { model | userForm = userForm }, FormUtils.scrollToInvalidField formMsg )


getRolesCompleted : UpdateConfig msg -> AppState -> Model -> Result ApiError (Pagination Role) -> ( Model, Cmd msg )
getRolesCompleted cfg appState model result =
    let
        newModel =
            case result of
                Ok pagination ->
                    { model | roles = ActionResult.Success pagination.items }

                Err error ->
                    { model | roles = ApiError.toActionResult appState (gettext "Unable to get the roles." appState.locale) error }

        cmd =
            RequestHelpers.getResultCmd cfg.logoutMsg result
    in
    ( newModel, cmd )


putUserCompleted : UpdateConfig msg -> AppState -> Model -> Result ApiError User -> ( Model, Cmd msg )
putUserCompleted cfg appState model result =
    case result of
        Ok user ->
            let
                updateCmd =
                    if Just user.uuid == Maybe.map .uuid appState.config.user then
                        Window.refresh ()

                    else
                        Cmd.none
            in
            ( { model | savingUser = ActionResult.Success <| gettext "Profile was successfully updated." appState.locale }
            , Cmd.batch
                [ Dom.scrollToTop ".Users__Edit__content"
                , updateCmd
                ]
            )

        Err err ->
            ( { model
                | savingUser = ApiError.toActionResult appState (gettext "Profile could not be saved." appState.locale) err
                , userForm = Form.setFormErrors appState err model.userForm
              }
            , Cmd.batch
                [ RequestHelpers.getResultCmd cfg.logoutMsg result
                , Dom.scrollToTop ".Users__Edit__content"
                ]
            )


view : AppState -> Model -> Html Msg
view appState model =
    let
        rolesActionResult =
            if UuidOrCurrent.isCurrent model.uuidOrCurrent then
                ActionResult.Success []

            else
                model.roles
    in
    Page.actionResultView appState (userView appState model) (ActionResult.combine model.user rolesActionResult)


userView : AppState -> Model -> ( User, List Role ) -> Html Msg
userView appState model ( user, roles ) =
    let
        content =
            if Admin.isEnabled appState.config.admin then
                readOnlyView appState user

            else
                Html.map EditFormMsg <|
                    userFormView appState model roles (UuidOrCurrent.isCurrent model.uuidOrCurrent)
    in
    div []
        [ Page.header (gettext "Profile" appState.locale) []
        , div [ class "row" ]
            [ content
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


userFormView : AppState -> Model -> List Role -> Bool -> Html Form.Msg
userFormView appState model roles isCurrent =
    let
        roleSelect =
            if isCurrent then
                Html.nothing

            else
                let
                    roleOptions =
                        Role.toFormOptions appState.locale roles
                in
                FormGroup.select appState.locale roleOptions model.userForm "role" <| gettext "Role" appState.locale

        activeToggle =
            if isCurrent then
                Html.nothing

            else
                FormGroup.toggle model.userForm "active" <| gettext "Active" appState.locale
    in
    Html.form [ onSubmit Form.Submit, class "col-8" ]
        [ FormResult.view model.savingUser
        , FormGroup.input appState.locale model.userForm "email" <| gettext "Email" appState.locale
        , FormGroup.input appState.locale model.userForm "firstName" <| gettext "First name" appState.locale
        , FormGroup.input appState.locale model.userForm "lastName" <| gettext "Last name" appState.locale
        , FormGroup.inputWithTypehints appState.config.organization.affiliations appState.locale model.userForm "affiliation" <| gettext "Affiliation" appState.locale
        , roleSelect
        , activeToggle
        , div [ class "mt-5" ]
            [ ActionButton.submit (ActionButton.SubmitConfig (gettext "Save" appState.locale) model.savingUser) ]
        ]


readOnlyView : AppState -> User -> Html msg
readOnlyView appState user =
    let
        editProfileUrl =
            AppState.getAdminClientUrl appState ++ "/users/edit/current"

        readOnlyInfo =
            div [ class "alert alert-info" ]
                [ faInfo
                , text (gettext "Your profile is managed elsewhere." appState.locale)
                , a
                    [ class "btn btn-primary ms-2"
                    , href editProfileUrl
                    ]
                    [ text (gettext "Edit profile" appState.locale)
                    , fa "fas fa-external-link-alt ms-2"
                    ]
                ]
    in
    div [ class "col-8" ]
        [ readOnlyInfo
        , FormGroup.readOnlyInput user.email (gettext "Email" appState.locale)
        , FormGroup.readOnlyInput user.firstName (gettext "First name" appState.locale)
        , FormGroup.readOnlyInput user.lastName (gettext "Last name" appState.locale)
        , FormGroup.readOnlyInput (Maybe.withDefault "" user.affiliation) (gettext "Affiliation" appState.locale)
        ]
