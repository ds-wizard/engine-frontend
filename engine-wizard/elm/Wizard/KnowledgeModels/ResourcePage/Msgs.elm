module Wizard.KnowledgeModels.ResourcePage.Msgs exposing (Msg(..))

import Shared.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.KnowledgeModel exposing (KnowledgeModel)


type Msg
    = FetchPreviewComplete (Result ApiError KnowledgeModel)
