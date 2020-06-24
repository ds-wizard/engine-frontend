module Wizard.KMEditor.Create.Msgs exposing (Msg(..))

import Form
import Shared.Data.Branch exposing (Branch)
import Shared.Data.Package exposing (Package)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = FormMsg Form.Msg
    | GetPackagesCompleted (Result ApiError (List Package))
    | PostBranchCompleted (Result ApiError Branch)
