module Wizard.Pages.Users.Edit.Models exposing
    ( Model
    , initialModel
    )

import Common.Data.UuidOrCurrent exposing (UuidOrCurrent)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Users.Edit.Components.ActiveSessions as ActiveSessions
import Wizard.Pages.Users.Edit.Components.ApiKeys as ApiKeys
import Wizard.Pages.Users.Edit.Components.AppKeys as AppKeys
import Wizard.Pages.Users.Edit.Components.Language as Language
import Wizard.Pages.Users.Edit.Components.Password as Password
import Wizard.Pages.Users.Edit.Components.Profile as Profile
import Wizard.Pages.Users.Edit.Components.SubmissionSettings as SubmissionSettings
import Wizard.Pages.Users.Edit.Components.Tours as Tours


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
    }
