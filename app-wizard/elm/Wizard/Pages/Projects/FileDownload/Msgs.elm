module Wizard.Pages.Projects.FileDownload.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.UrlResponse exposing (UrlResponse)


type Msg
    = GotFileUrlCompleted (Result ApiError UrlResponse)
