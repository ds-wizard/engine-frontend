module Wizard.Locales.Create.Msgs exposing (Msg(..))

import Form
import Json.Decode as D
import Shared.Data.ApiError exposing (ApiError)
import Wizard.Common.Components.Dropzone as Dropzone


type Msg
    = WizardContentFileDropzoneMsg Dropzone.Msg
    | MailContentFileDropzoneMsg Dropzone.Msg
    | CreateCompleted (Result ApiError ())
    | Cancel
    | FormMsg Form.Msg
    | LocaleConverted D.Value
