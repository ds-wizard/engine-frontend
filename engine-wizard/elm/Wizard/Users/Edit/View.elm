module Wizard.Users.Edit.View exposing (view)

import Form exposing (Form)
import Form.Input as Input
import Gettext exposing (gettext)
import Html exposing (Html, a, div, h4, img, p, strong, text)
import Html.Attributes exposing (class, classList, src)
import Html.Events exposing (onClick, onSubmit)
import Shared.Auth.Role as Role
import Shared.Data.User as User exposing (User)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode)
import Shared.Markdown as Markdown
import String.Format as String
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy, detailClass, wideDetailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.ExternalLoginButton as ExternalLoginButton
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Users.Common.UserEditForm exposing (UserEditForm)
import Wizard.Users.Common.UserPasswordForm exposing (UserPasswordForm)
import Wizard.Users.Edit.Models exposing (Model, View(..))
import Wizard.Users.Edit.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (profileView appState model) model.user


profileView : AppState -> Model -> User -> Html Msg
profileView appState model user =
    div [ class "Users__Edit col-full" ]
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
            , dataCy "user_nav_profile"
            ]
            [ text (gettext "Profile" appState.locale)
            ]
        , a
            [ class "nav-link"
            , classList [ ( "active", model.currentView == Password ) ]
            , onClick <| ChangeView Password
            , dataCy "user_nav_password"
            ]
            [ text (gettext "Password" appState.locale)
            ]
        ]


userView : AppState -> Model -> User -> Html Msg
userView appState model user =
    if model.currentView /= Profile then
        emptyNode

    else
        div [ wideDetailClass "" ]
            [ Page.header (gettext "Profile" appState.locale) []
            , div [ class "row" ]
                [ Html.form [ onSubmit (EditFormMsg Form.Submit), class "col-8" ]
                    [ FormResult.view appState model.savingUser
                    , userFormView appState user model.userForm (model.uuid == "current") |> Html.map EditFormMsg
                    , div [ class "mt-5" ]
                        [ ActionButton.submit appState (ActionButton.SubmitConfig (gettext "Save" appState.locale) model.savingUser) ]
                    ]
                , div [ class "col-4" ]
                    [ div [ class "col-user-image" ]
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
userFormView appState user form current =
    let
        roleSelect =
            if current then
                emptyNode

            else
                FormGroup.select appState (Role.options appState) form "role" <| gettext "Role" appState.locale

        activeToggle =
            if current then
                emptyNode

            else
                FormGroup.toggle form "active" <| gettext "Active" appState.locale

        submissionPropsIndexes =
            Form.getListIndexes "submissionProps" form

        submissionSettings =
            if current && appState.config.submission.enabled && List.length submissionPropsIndexes > 0 then
                div [ class "mt-5" ]
                    (h4 [] [ text (gettext "Submission Settings" appState.locale) ]
                        :: List.map submissionSettingsSection submissionPropsIndexes
                    )

            else
                emptyNode

        submissionSettingsSection i =
            let
                field name =
                    "submissionProps." ++ String.fromInt i ++ "." ++ name

                sectionName =
                    Maybe.withDefault "" (Form.getFieldAsString (field "name") form).value

                valueIndexes =
                    Form.getListIndexes (field "values") form

                sectionContent =
                    if List.length valueIndexes > 0 then
                        div []
                            (List.map (submissionSettingsSectionProp (field "values")) valueIndexes)

                    else
                        p [ class "text-muted" ] [ text <| String.format (gettext "There is no settings for %s." appState.locale) [ sectionName ] ]
            in
            div [ class "mb-4" ]
                [ strong [] [ text sectionName ]
                , sectionContent
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
    in
    div []
        [ FormGroup.input appState form "email" <| gettext "Email" appState.locale
        , FormExtra.blockAfter (List.map (ExternalLoginButton.badgeWrapper appState) user.sources)
        , FormGroup.input appState form "firstName" <| gettext "First name" appState.locale
        , FormGroup.input appState form "lastName" <| gettext "Last name" appState.locale
        , FormGroup.inputWithTypehints appState.config.organization.affiliations appState form "affiliation" <| gettext "Affiliation" appState.locale
        , roleSelect
        , activeToggle
        , submissionSettings
        ]


passwordView : AppState -> Model -> Html Msg
passwordView appState model =
    if model.currentView /= Password then
        emptyNode

    else
        Html.form [ onSubmit (PasswordFormMsg Form.Submit), detailClass "" ]
            [ Page.header (gettext "Password" appState.locale) []
            , FormResult.view appState model.savingPassword
            , passwordFormView appState model.passwordForm |> Html.map PasswordFormMsg
            , div [ class "mt-5" ]
                [ ActionButton.submit appState (ActionButton.SubmitConfig (gettext "Save" appState.locale) model.savingPassword) ]
            ]


passwordFormView : AppState -> Form FormError UserPasswordForm -> Html Form.Msg
passwordFormView appState form =
    div []
        [ FormGroup.passwordWithStrength appState form "password" <| gettext "New password" appState.locale
        , FormGroup.password appState form "passwordConfirmation" <| gettext "New password again" appState.locale
        ]
