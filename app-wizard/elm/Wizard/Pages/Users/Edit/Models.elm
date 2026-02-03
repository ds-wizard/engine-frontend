module Wizard.Pages.Users.Edit.Models exposing
    ( Model
    , initLocalModel
    , initialModel
    )

import Common.Data.UuidOrCurrent exposing (UuidOrCurrent)
import Uuid
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Users.Edit.Components.ActiveSessions as ActiveSessions
import Wizard.Pages.Users.Edit.Components.ApiKeys as ApiKeys
import Wizard.Pages.Users.Edit.Components.AppKeys as AppKeys
import Wizard.Pages.Users.Edit.Components.Language as Language
import Wizard.Pages.Users.Edit.Components.Password as Password
import Wizard.Pages.Users.Edit.Components.PluginSettings as PluginSettings
import Wizard.Pages.Users.Edit.Components.Profile as Profile
import Wizard.Pages.Users.Edit.Components.SubmissionSettings as SubmissionSettings
import Wizard.Pages.Users.Edit.Components.Tours as Tours
import Wizard.Pages.Users.Edit.UserEditRoutes as UserEditRoute exposing (UserEditRoute)


type alias Model =
    { uuidOrCurrent : UuidOrCurrent
    , profileModel : Profile.Model
    , passwordModel : Password.Model
    , languageModel : Language.Model
    , toursModel : Tours.Model
    , apiKeysModel : ApiKeys.Model
    , appKeysModel : AppKeys.Model
    , activeSessionsModel : ActiveSessions.Model
    , submissionSettingsModel : SubmissionSettings.Model
    , pluginSettingsModel : PluginSettings.Model
    }


initialModel : AppState -> UuidOrCurrent -> Model
initialModel appState uuidOrEmpty =
    { uuidOrCurrent = uuidOrEmpty
    , profileModel = Profile.initialModel uuidOrEmpty
    , passwordModel = Password.initialModel appState uuidOrEmpty
    , languageModel = Language.initialModel
    , toursModel = Tours.initialModel
    , apiKeysModel = ApiKeys.initialModel uuidOrEmpty
    , appKeysModel = AppKeys.initialModel uuidOrEmpty
    , activeSessionsModel = ActiveSessions.initialModel
    , submissionSettingsModel = SubmissionSettings.initialModel
    , pluginSettingsModel = PluginSettings.initialModel uuidOrEmpty Uuid.nil
    }


initLocalModel : AppState -> UserEditRoute -> UuidOrCurrent -> Model -> Model
initLocalModel appState userEditRoute uuidOrCurrent model =
    let
        updatedModel =
            case userEditRoute of
                UserEditRoute.Profile ->
                    { model | profileModel = Profile.initialModel uuidOrCurrent }

                UserEditRoute.Password ->
                    { model | passwordModel = Password.initialModel appState uuidOrCurrent }

                UserEditRoute.Language ->
                    { model | languageModel = Language.initialModel }

                UserEditRoute.Tours ->
                    { model | toursModel = Tours.initialModel }

                UserEditRoute.ApiKeys ->
                    { model | apiKeysModel = ApiKeys.initialModel uuidOrCurrent }

                UserEditRoute.AppKeys ->
                    { model | appKeysModel = AppKeys.initialModel uuidOrCurrent }

                UserEditRoute.ActiveSessions ->
                    { model | activeSessionsModel = ActiveSessions.initialModel }

                UserEditRoute.SubmissionSettings ->
                    { model | submissionSettingsModel = SubmissionSettings.initialModel }

                UserEditRoute.PluginSettings pluginUuid ->
                    { model | pluginSettingsModel = PluginSettings.initialModel uuidOrCurrent pluginUuid }
    in
    { updatedModel | uuidOrCurrent = uuidOrCurrent }
