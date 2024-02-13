module Wizard.Users.Edit.Models exposing
    ( Model
    , initialModel
    )

import Shared.Common.UuidOrCurrent exposing (UuidOrCurrent)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Users.Edit.Components.ActiveSessions as ActiveSessions
import Wizard.Users.Edit.Components.ApiKeys as ApiKeys
import Wizard.Users.Edit.Components.AppKeys as AppKeys
import Wizard.Users.Edit.Components.Password as Password
import Wizard.Users.Edit.Components.Profile as Profile
import Wizard.Users.Edit.Components.SubmissionSettings as SubmissionSettings


type alias Model =
    { uuidOrCurrent : UuidOrCurrent
    , profileModel : Profile.Model
    , passwordModel : Password.Model
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
    , apiKeysModel = ApiKeys.initialModel uuidOrEmpty
    , appKeysModel = AppKeys.initialModel uuidOrEmpty
    , activeSessionsModel = ActiveSessions.initialModel
    , submissionSettingsModel = SubmissionSettings.initialModel
    }
