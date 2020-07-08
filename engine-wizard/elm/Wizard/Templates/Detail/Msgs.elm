module Wizard.Templates.Detail.Msgs exposing (Msg(..))

import Shared.Data.TemplateDetail exposing (TemplateDetail)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = GetTemplateCompleted (Result ApiError TemplateDetail)
    | ShowDeleteDialog Bool
    | DeleteVersion
    | DeleteVersionCompleted (Result ApiError ())
