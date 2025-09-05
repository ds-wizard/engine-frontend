module Wizard.Pages.Projects.FileDownload.Msgs exposing (Msg(..))

import Common.Api.Models.UrlResponse exposing (UrlResponse)
import Common.Data.ApiError exposing (ApiError)


type Msg
    = GotFileUrlCompleted (Result ApiError UrlResponse)
