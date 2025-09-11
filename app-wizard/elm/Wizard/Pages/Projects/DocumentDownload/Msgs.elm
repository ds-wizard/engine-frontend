module Wizard.Pages.Projects.DocumentDownload.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.UrlResponse exposing (UrlResponse)


type Msg
    = GotDocumentUrlCompleted (Result ApiError UrlResponse)
