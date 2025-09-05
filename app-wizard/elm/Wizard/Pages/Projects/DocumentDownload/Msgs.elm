module Wizard.Pages.Projects.DocumentDownload.Msgs exposing (Msg(..))

import Common.Api.Models.UrlResponse exposing (UrlResponse)
import Common.Data.ApiError exposing (ApiError)


type Msg
    = GotDocumentUrlCompleted (Result ApiError UrlResponse)
