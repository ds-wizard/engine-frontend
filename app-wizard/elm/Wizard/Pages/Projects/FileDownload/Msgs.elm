module Wizard.Pages.Projects.FileDownload.Msgs exposing (Msg(..))

import Shared.Api.Models.UrlResponse exposing (UrlResponse)
import Shared.Data.ApiError exposing (ApiError)


type Msg
    = GotFileUrlCompleted (Result ApiError UrlResponse)
