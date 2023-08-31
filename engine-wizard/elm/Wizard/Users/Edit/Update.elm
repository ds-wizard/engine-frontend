module Wizard.Users.Edit.Update exposing (fetchData, update)

import Shared.Common.UuidOrCurrent exposing (UuidOrCurrent)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Users.Edit.Components.ActiveSessions as ActiveSessions
import Wizard.Users.Edit.Components.ApiKeys as ApiKeys
import Wizard.Users.Edit.Components.Password as Password
import Wizard.Users.Edit.Components.Profile as Profile
import Wizard.Users.Edit.Components.SubmissionSettings as SubmissionSettings
import Wizard.Users.Edit.Models exposing (Model)
import Wizard.Users.Edit.Msgs exposing (Msg(..))
import Wizard.Users.Edit.UserEditRoutes as UserEditRoute exposing (UserEditRoute)


fetchData : AppState -> UuidOrCurrent -> UserEditRoute -> Cmd Msg
fetchData appState uuidOrCurrent subroute =
    case subroute of
        UserEditRoute.Profile ->
            Cmd.map ProfileMsg (Profile.fetchData appState uuidOrCurrent)

        UserEditRoute.Password ->
            Cmd.none

        UserEditRoute.ApiKeys ->
            Cmd.map ApiKeysMsg (ApiKeys.fetchData appState)

        UserEditRoute.ActiveSessions ->
            Cmd.map ActiveSessionsMsg (ActiveSessions.fetchData appState)

        UserEditRoute.SubmissionSettings ->
            Cmd.map SubmissionSettingsMsg (SubmissionSettings.fetchData appState)


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        ProfileMsg profileMsg ->
            let
                updateConfig =
                    { wrapMsg = wrapMsg << ProfileMsg
                    , logoutMsg = Wizard.Msgs.logoutMsg
                    , updateUserMsg = Wizard.Msgs.updateUserMsg
                    }

                ( profileModel, profileCmd ) =
                    Profile.update updateConfig appState profileMsg model.profileModel
            in
            ( { model | profileModel = profileModel }, profileCmd )

        PasswordMsg passwordMsg ->
            let
                updateConfig =
                    { wrapMsg = wrapMsg << PasswordMsg
                    , logoutMsg = Wizard.Msgs.logoutMsg
                    }

                ( passwordModel, passwordCmd ) =
                    Password.update updateConfig appState passwordMsg model.passwordModel
            in
            ( { model | passwordModel = passwordModel }, passwordCmd )

        ApiKeysMsg apiKeysMsg ->
            let
                updateConfig =
                    { wrapMsg = wrapMsg << ApiKeysMsg
                    , logoutMsg = Wizard.Msgs.logoutMsg
                    }

                ( apiKeysModel, apiKeysCmd ) =
                    ApiKeys.update updateConfig appState apiKeysMsg model.apiKeysModel
            in
            ( { model | apiKeysModel = apiKeysModel }, apiKeysCmd )

        ActiveSessionsMsg activeSessionsMsg ->
            let
                updateConfig =
                    { wrapMsg = wrapMsg << ActiveSessionsMsg
                    , logoutMsg = Wizard.Msgs.logoutMsg
                    }

                ( activeSessionsModel, activeSessionCmd ) =
                    ActiveSessions.update updateConfig appState activeSessionsMsg model.activeSessionsModel
            in
            ( { model | activeSessionsModel = activeSessionsModel }, activeSessionCmd )

        SubmissionSettingsMsg submissionSettingsMsg ->
            let
                updateConfig =
                    { wrapMsg = wrapMsg << SubmissionSettingsMsg
                    , logoutMsg = Wizard.Msgs.logoutMsg
                    }

                ( submissionSettingsModel, submissionSettingsCmd ) =
                    SubmissionSettings.update updateConfig appState submissionSettingsMsg model.submissionSettingsModel
            in
            ( { model | submissionSettingsModel = submissionSettingsModel }, submissionSettingsCmd )
