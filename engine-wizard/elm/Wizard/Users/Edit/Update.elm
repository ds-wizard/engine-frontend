module Wizard.Users.Edit.Update exposing (fetchData, update)

import Shared.Common.UuidOrCurrent exposing (UuidOrCurrent)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Users.Edit.Components.ActiveSessions as ActiveSessions
import Wizard.Users.Edit.Components.ApiKeys as ApiKeys
import Wizard.Users.Edit.Components.AppKeys as AppKeys
import Wizard.Users.Edit.Components.Language as Language
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

        UserEditRoute.Language ->
            Cmd.map LanguageMsg (Language.fetchData appState)

        UserEditRoute.ApiKeys ->
            Cmd.map ApiKeysMsg (ApiKeys.fetchData appState)

        UserEditRoute.AppKeys ->
            Cmd.map AppKeysMsg (AppKeys.fetchData appState)

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

        LanguageMsg languageMsg ->
            let
                updateConfig =
                    { wrapMsg = wrapMsg << LanguageMsg
                    , logoutMsg = Wizard.Msgs.logoutMsg
                    }

                ( languageModel, languageCmd ) =
                    Language.update updateConfig appState languageMsg model.languageModel
            in
            ( { model | languageModel = languageModel }, languageCmd )

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

        AppKeysMsg appKeysMsg ->
            let
                updateConfig =
                    { wrapMsg = wrapMsg << AppKeysMsg
                    , logoutMsg = Wizard.Msgs.logoutMsg
                    }

                ( appKeysModel, appKeysCmd ) =
                    AppKeys.update updateConfig appState appKeysMsg model.appKeysModel
            in
            ( { model | appKeysModel = appKeysModel }, appKeysCmd )

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
