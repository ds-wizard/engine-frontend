module Wizard.Users.Edit.View exposing (view)

import Form exposing (Form)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Shared.Locale exposing (l, lg, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.Html exposing (emptyNode)
import Wizard.Common.Html.Attribute exposing (detailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Routes as Routes
import Wizard.Users.Common.User as User exposing (User)
import Wizard.Users.Common.UserEditForm exposing (UserEditForm)
import Wizard.Users.Common.UserPasswordForm exposing (UserPasswordForm)
import Wizard.Users.Edit.Models exposing (..)
import Wizard.Users.Edit.Msgs exposing (Msg(..))
import Wizard.Users.Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Users.Edit.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Users.Edit.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (profileView appState model) model.user


profileView : AppState -> Model -> User -> Html Msg
profileView appState model _ =
    div [ detailClass "Users__Edit" ]
        [ Page.header (l_ "header.title" appState) []
        , div []
            [ navbar appState model
            , userView appState model
            , passwordView appState model
            ]
        ]


navbar : AppState -> Model -> Html Msg
navbar appState model =
    ul [ class "nav nav-tabs" ]
        [ li [ class "nav-item" ]
            [ a
                [ class "nav-link"
                , classList [ ( "active", model.currentView == Profile ) ]
                , onClick <| ChangeView Profile
                ]
                [ lx_ "navbar.profile" appState ]
            ]
        , li [ class "nav-item" ]
            [ a
                [ class "nav-link"
                , classList [ ( "active", model.currentView == Password ) ]
                , onClick <| ChangeView Password
                ]
                [ lx_ "navbar.password" appState ]
            ]
        ]


userView : AppState -> Model -> Html Msg
userView appState model =
    div [ classList [ ( "hidden", model.currentView /= Profile ) ] ]
        [ FormResult.view appState model.savingUser
        , userFormView appState model.userForm (model.uuid == "current") |> Html.map EditFormMsg
        , formActionsView appState model (ActionButton.ButtonConfig (l_ "userView.save" appState) model.savingUser (EditFormMsg Form.Submit) False)
        ]


userFormView : AppState -> Form CustomFormError UserEditForm -> Bool -> Html Form.Msg
userFormView appState form current =
    let
        roleOptions =
            ( "", "--" ) :: List.map (\o -> ( o, o )) User.roles

        roleSelect =
            if current then
                emptyNode

            else
                FormGroup.select appState roleOptions form "role" <| lg "user.role" appState

        activeToggle =
            if current then
                emptyNode

            else
                FormGroup.toggle form "active" <| lg "user.active" appState

        formHtml =
            div []
                [ FormGroup.input appState form "email" <| lg "user.email" appState
                , FormGroup.input appState form "firstName" <| lg "user.firstName" appState
                , FormGroup.input appState form "lastName" <| lg "user.lastName" appState
                , roleSelect
                , activeToggle
                ]
    in
    formHtml


passwordView : AppState -> Model -> Html Msg
passwordView appState model =
    div [ classList [ ( "hidden", model.currentView /= Password ) ] ]
        [ FormResult.view appState model.savingPassword
        , passwordFormView appState model.passwordForm |> Html.map PasswordFormMsg
        , formActionsView appState
            model
            (ActionButton.ButtonConfig (l_ "passwordView.save" appState) model.savingPassword (PasswordFormMsg Form.Submit) False)
        ]


passwordFormView : AppState -> Form CustomFormError UserPasswordForm -> Html Form.Msg
passwordFormView appState form =
    div []
        [ FormGroup.password appState form "password" <| l_ "passwordForm.password" appState
        , FormGroup.password appState form "passwordConfirmation" <| l_ "passwordForm.passwordConfirmation" appState
        ]


formActionsView : AppState -> Model -> ActionButton.ButtonConfig a Msg -> Html Msg
formActionsView appState { uuid } actionButtonConfig =
    case uuid of
        "current" ->
            FormActions.viewActionOnly appState actionButtonConfig

        _ ->
            FormActions.view appState (Routes.UsersRoute Wizard.Users.Routes.IndexRoute) actionButtonConfig
