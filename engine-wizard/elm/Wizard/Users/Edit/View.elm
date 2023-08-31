module Wizard.Users.Edit.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, div, strong, text)
import Html.Attributes exposing (class, classList)
import Html.Extra as Html
import Maybe.Extra as Maybe
import Shared.Common.UuidOrCurrent as UuidOrCurrent
import Uuid
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Routes as Routes
import Wizard.Users.Edit.Components.ActiveSessions as ActiveSessions
import Wizard.Users.Edit.Components.ApiKeys as ApiKeys
import Wizard.Users.Edit.Components.Password as Password
import Wizard.Users.Edit.Components.Profile as Profile
import Wizard.Users.Edit.Components.SubmissionSettings as SubmissionSettings
import Wizard.Users.Edit.Models exposing (Model)
import Wizard.Users.Edit.Msgs exposing (Msg(..))
import Wizard.Users.Edit.UserEditRoutes as UserEditRoutes exposing (UserEditRoute)


view : AppState -> UserEditRoute -> Model -> Html Msg
view appState subroute model =
    let
        content =
            case subroute of
                UserEditRoutes.Profile ->
                    Html.map ProfileMsg <|
                        Profile.view appState model.profileModel

                UserEditRoutes.Password ->
                    Html.map PasswordMsg <|
                        Password.view appState model.passwordModel

                UserEditRoutes.ApiKeys ->
                    Html.map ApiKeysMsg <|
                        ApiKeys.view appState model.apiKeysModel

                UserEditRoutes.ActiveSessions ->
                    Html.map ActiveSessionsMsg <|
                        ActiveSessions.view appState model.activeSessionsModel

                UserEditRoutes.SubmissionSettings ->
                    Html.map SubmissionSettingsMsg <|
                        SubmissionSettings.view appState model.submissionSettingsModel
    in
    div [ class "Users__Edit col-full" ]
        [ div [ class "Users__Edit__navigation" ] [ navigation appState subroute model ]
        , div [ class "Users__Edit__content" ]
            [ content ]
        ]


navigation : AppState -> UserEditRoute -> Model -> Html Msg
navigation appState subroute model =
    let
        isCurrent =
            UuidOrCurrent.isCurrent model.uuidOrCurrent || UuidOrCurrent.matchUuid model.uuidOrCurrent (Maybe.unwrap Uuid.nil .uuid appState.session.user)
    in
    div [ class "nav nav-pills flex-column" ]
        [ strong [] [ text (gettext "User Settings" appState.locale) ]
        , linkTo appState
            (Routes.usersEdit model.uuidOrCurrent)
            [ class "nav-link"
            , classList [ ( "active", subroute == UserEditRoutes.Profile ) ]
            , dataCy "user_nav_profile"
            ]
            [ text (gettext "Profile" appState.locale)
            ]
        , linkTo appState
            (Routes.usersEditPassword model.uuidOrCurrent)
            [ class "nav-link"
            , classList [ ( "active", subroute == UserEditRoutes.Password ) ]
            , dataCy "user_nav_password"
            ]
            [ text (gettext "Password" appState.locale)
            ]
        , Html.viewIf isCurrent
            (linkTo appState
                (Routes.usersEditApiKeys model.uuidOrCurrent)
                [ class "nav-link"
                , classList [ ( "active", subroute == UserEditRoutes.ApiKeys ) ]
                , dataCy "user_nav_api-keys"
                ]
                [ text (gettext "API Keys" appState.locale)
                ]
            )
        , Html.viewIf isCurrent
            (linkTo appState
                (Routes.usersEditActiveSessions model.uuidOrCurrent)
                [ class "nav-link"
                , classList [ ( "active", subroute == UserEditRoutes.ActiveSessions ) ]
                , dataCy "user_nav_active-sessions"
                ]
                [ text (gettext "Active Sessions" appState.locale)
                ]
            )
        , Html.viewIf isCurrent
            (linkTo appState
                (Routes.usersEditSubmissionSettings model.uuidOrCurrent)
                [ class "nav-link"
                , classList [ ( "active", subroute == UserEditRoutes.SubmissionSettings ) ]
                , dataCy "user_nav_submission-settings"
                ]
                [ text (gettext "Submission Settings" appState.locale)
                ]
            )
        ]
