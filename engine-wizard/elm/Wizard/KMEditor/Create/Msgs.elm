module Wizard.KMEditor.Create.Msgs exposing (Msg(..))

import Form
import Shared.Error.ApiError exposing (ApiError)
import Wizard.KMEditor.Common.Branch exposing (Branch)
import Wizard.KnowledgeModels.Common.Package exposing (Package)


type Msg
    = FormMsg Form.Msg
    | GetPackagesCompleted (Result ApiError (List Package))
    | PostBranchCompleted (Result ApiError Branch)
