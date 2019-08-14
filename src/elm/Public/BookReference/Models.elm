module Public.BookReference.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Public.Common.BookReference exposing (BookReference)


type alias Model =
    { bookReference : ActionResult BookReference
    }


initialModel : Model
initialModel =
    { bookReference = Loading
    }
