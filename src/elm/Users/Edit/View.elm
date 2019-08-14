module Users.Edit.View exposing (view)

import Common.AppState exposing (AppState)
import Common.Form exposing (CustomFormError)
import Common.Html exposing (emptyNode)
import Common.Html.Attribute exposing (detailClass)
import Common.Locale exposing (l, lg, lx)
import Common.View.ActionButton as ActionButton
import Common.View.FormActions as FormActions
import Common.View.FormGroup as FormGroup
import Common.View.FormResult as FormResult
import Common.View.Page as Page
import Form exposing (Form)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Routes
import Users.Common.User as User exposing (User)
import Users.Common.UserEditForm exposing (UserEditForm)
import Users.Common.UserPasswordForm exposing (UserPasswordForm)
import Users.Edit.Models exposing (..)
import Users.Edit.Msgs exposing (Msg(..))
import Users.Routes


l_ : String -> AppState -> String
l_ =
    l "Users.Edit.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Users.Edit.View"


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
        [ FormResult.view model.savingUser
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
                , FormGroup.input appState form "name" <| lg "user.name" appState
                , FormGroup.input appState form "surname" <| lg "user.surname" appState
                , roleSelect
                , activeToggle
                ]
    in
    formHtml


passwordView : AppState -> Model -> Html Msg
passwordView appState model =
    div [ classList [ ( "hidden", model.currentView /= Password ) ] ]
        [ FormResult.view model.savingPassword
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
            FormActions.viewActionOnly actionButtonConfig

        _ ->
            FormActions.view appState (Routes.UsersRoute Users.Routes.IndexRoute) actionButtonConfig
