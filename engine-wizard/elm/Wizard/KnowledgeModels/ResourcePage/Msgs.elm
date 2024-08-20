module Wizard.KnowledgeModels.ResourcePage.Msgs exposing (Msg(..))

import Shared.Data.KnowledgeModel exposing (KnowledgeModel)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = FetchPreviewComplete (Result ApiError KnowledgeModel)
