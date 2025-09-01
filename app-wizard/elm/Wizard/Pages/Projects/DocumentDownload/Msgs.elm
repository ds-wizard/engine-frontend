module Wizard.Pages.Projects.DocumentDownload.Msgs exposing (Msg(..))

import Shared.Api.Models.UrlResponse exposing (UrlResponse)
import Shared.Data.ApiError exposing (ApiError)


type Msg
    = GotDocumentUrlCompleted (Result ApiError UrlResponse)
