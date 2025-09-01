module Wizard.Projects.FileDownload.Msgs exposing (Msg(..))

import Shared.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.UrlResponse exposing (UrlResponse)


type Msg
    = GotFileUrlCompleted (Result ApiError UrlResponse)
