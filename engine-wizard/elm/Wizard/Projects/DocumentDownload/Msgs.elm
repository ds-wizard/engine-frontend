module Wizard.Projects.DocumentDownload.Msgs exposing (Msg(..))

import Shared.Data.UrlResponse exposing (UrlResponse)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = GotDocumentUrlCompleted (Result ApiError UrlResponse)
