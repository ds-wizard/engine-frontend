module Wizard.Locales.Detail.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Shared.Data.LocaleDetail exposing (LocaleDetail)


type alias Model =
    { id : String
    , locale : ActionResult LocaleDetail
    , deletingVersion : ActionResult String
    , showDeleteDialog : Bool
    }


initialModel : String -> Model
initialModel id =
    { id = id
    , locale = Loading
    , deletingVersion = Unset
    , showDeleteDialog = False
    }
