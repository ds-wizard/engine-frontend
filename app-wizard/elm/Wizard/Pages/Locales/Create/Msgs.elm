module Wizard.Pages.Locales.Create.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Form
import Json.Decode as D
import Wizard.Components.Dropzone as Dropzone


type Msg
    = WizardContentFileDropzoneMsg Dropzone.Msg
    | MailContentFileDropzoneMsg Dropzone.Msg
    | CreateCompleted (Result ApiError ())
    | Cancel
    | FormMsg Form.Msg
    | LocaleConverted D.Value
