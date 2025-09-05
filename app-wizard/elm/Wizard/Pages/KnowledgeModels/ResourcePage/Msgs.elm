module Wizard.Pages.KnowledgeModels.ResourcePage.Msgs exposing (Msg(..))

import Common.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.KnowledgeModel exposing (KnowledgeModel)


type Msg
    = FetchPreviewComplete (Result ApiError KnowledgeModel)
