module Wizard.Templates.Index.Msgs exposing (Msg(..))

import Shared.Data.Template exposing (Template)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.Listing as Listing


type Msg
    = GetTemplatesCompleted (Result ApiError (List Template))
    | ShowHideDeleteTemplate (Maybe Template)
    | DeleteTemplate
    | DeleteTemplateCompleted (Result ApiError ())
    | ListingMsg Listing.Msg
