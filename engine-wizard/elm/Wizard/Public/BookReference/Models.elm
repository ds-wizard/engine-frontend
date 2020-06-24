module Wizard.Public.BookReference.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Shared.Data.BookReference exposing (BookReference)


type alias Model =
    { bookReference : ActionResult BookReference
    }


initialModel : Model
initialModel =
    { bookReference = Loading
    }
