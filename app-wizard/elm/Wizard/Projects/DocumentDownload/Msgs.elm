module Wizard.Projects.DocumentDownload.Msgs exposing (Msg(..))

import Shared.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.UrlResponse exposing (UrlResponse)


type Msg
    = GotDocumentUrlCompleted (Result ApiError UrlResponse)
