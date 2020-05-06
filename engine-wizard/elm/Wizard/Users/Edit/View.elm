module Wizard.Users.Edit.View exposing (view)

import Form exposing (Form)
import Form.Input as Input
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Markdown
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l, lg, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.Html.Attribute exposing (detailClass, wideDetailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.ExternalLoginButton as ExternalLoginButton
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Users.Common.Role as Role
import Wizard.Users.Common.User as User exposing (User)
import Wizard.Users.Common.UserEditForm exposing (UserEditForm)
import Wizard.Users.Common.UserPasswordForm exposing (UserPasswordForm)
import Wizard.Users.Edit.Models exposing (..)
import Wizard.Users.Edit.Msgs exposing (Msg(..))


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
profileView appState model user =
    div [ class "Users__Edit" ]
        [ div [ class "Users__Edit__navigation" ] [ navigation appState model ]
        , div [ class "Users__Edit__content" ]
            [ userView appState model user
            , passwordView appState model
            ]
        ]


navigation : AppState -> Model -> Html Msg
navigation appState model =
    div [ class "nav nav-pills flex-column" ]
        [ strong [] [ text "User settings" ]
        , a
            [ class "nav-link"
            , classList [ ( "active", model.currentView == Profile ) ]
            , onClick <| ChangeView Profile
            ]
            [ lx_ "navbar.profile" appState
            ]
        , a
            [ class "nav-link"
            , classList [ ( "active", model.currentView == Password ) ]
            , onClick <| ChangeView Password
            ]
            [ lx_ "navbar.password" appState
            ]
        ]


userView : AppState -> Model -> User -> Html Msg
userView appState model user =
    div [ wideDetailClass "", classList [ ( "hidden", model.currentView /= Profile ) ] ]
        [ Page.header (l_ "navbar.profile" appState) []
        , div [ class "row" ]
            [ div [ class "col-8" ]
                [ FormResult.view appState model.savingUser
                ]
            ]
        , div [ class "row" ]
            [ div [ class "col-8" ]
                [ userFormView appState user model.userForm (model.uuid == "current") |> Html.map EditFormMsg
                , div [ class "mt-5" ]
                    [ ActionButton.button appState (ActionButton.ButtonConfig (l_ "userView.save" appState) model.savingUser (EditFormMsg Form.Submit) False) ]
                ]
            , div [ class "col-4" ]
                [ div [ class "col-user-image" ]
                    [ strong [] [ lx_ "userView.userImage" appState ]
                    , div []
                        [ img [ src (User.imageUrl user), class "user-icon user-icon-large" ] []
                        ]
                    , Markdown.toHtml [ class "text-muted" ] (l_ "userView.userImage.desc" appState)
                    ]
                ]
            ]
        ]


userFormView : AppState -> User -> Form CustomFormError UserEditForm -> Bool -> Html Form.Msg
userFormView appState user form current =
    let
        roleSelect =
            if current then
                emptyNode

            else
                FormGroup.select appState (Role.options appState) form "role" <| lg "user.role" appState

        activeToggle =
            if current then
                emptyNode

            else
                FormGroup.toggle form "active" <| lg "user.active" appState

        submissionSettings =
            if current && appState.config.submission.enabled then
                div [ class "mt-5" ]
                    ([ h4 [] [ lx_ "submissionSettings.title" appState ] ]
                        ++ List.map submissionSettingsSection (Form.getListIndexes "submissionProps" form)
                    )

            else
                emptyNode

        submissionSettingsSection i =
            let
                field name =
                    "submissionProps." ++ String.fromInt i ++ "." ++ name

                sectionName =
                    Maybe.withDefault "" (Form.getFieldAsString (field "name") form).value
            in
            div [ class "mb-4" ]
                [ strong [] [ text sectionName ]
                , div []
                    (List.map (submissionSettingsSectionProp (field "values")) (Form.getListIndexes (field "values") form))
                ]

        submissionSettingsSectionProp prefix i =
            let
                field name =
                    prefix ++ "." ++ String.fromInt i ++ "." ++ name

                valueField =
                    Form.getFieldAsString (field "value") form

                propName =
                    Maybe.withDefault "" (Form.getFieldAsString (field "key") form).value
            in
            div [ class "row mb-1" ]
                [ div [ class "col-4 d-flex align-items-center" ] [ text propName ]
                , div [ class "col-8" ] [ Input.textInput valueField [ class "form-control" ] ]
                ]

        formHtml =
            div []
                [ FormGroup.input appState form "email" <| lg "user.email" appState
                , FormExtra.blockAfter (List.map (ExternalLoginButton.badgeWrapper appState) user.sources)
                , FormGroup.input appState form "firstName" <| lg "user.firstName" appState
                , FormGroup.input appState form "lastName" <| lg "user.lastName" appState
                , FormGroup.inputWithTypehints appState.config.organization.affiliations appState form "affiliation" <| lg "user.affiliation" appState
                , roleSelect
                , activeToggle
                , submissionSettings
                ]
    in
    formHtml


passwordView : AppState -> Model -> Html Msg
passwordView appState model =
    div [ detailClass "", classList [ ( "hidden", model.currentView /= Password ) ] ]
        [ Page.header (l_ "navbar.password" appState) []
        , FormResult.view appState model.savingPassword
        , passwordFormView appState model.passwordForm |> Html.map PasswordFormMsg
        , div [ class "mt-5" ]
            [ ActionButton.button appState (ActionButton.ButtonConfig (l_ "passwordView.save" appState) model.savingPassword (PasswordFormMsg Form.Submit) False) ]
        ]


passwordFormView : AppState -> Form CustomFormError UserPasswordForm -> Html Form.Msg
passwordFormView appState form =
    div []
        [ FormGroup.password appState form "password" <| l_ "passwordForm.password" appState
        , FormGroup.password appState form "passwordConfirmation" <| l_ "passwordForm.passwordConfirmation" appState
        ]
