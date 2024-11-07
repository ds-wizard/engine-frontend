module Wizard.Projects.FileDownload.Msgs exposing (Msg(..))

import Shared.Data.UrlResponse exposing (UrlResponse)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = GotFileUrlCompleted (Result ApiError UrlResponse)
