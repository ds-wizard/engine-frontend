module Public.BookReference.Models exposing (BookReference, Model, bookReferenceDecoder, initialModel)

import ActionResult exposing (ActionResult(..))
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (required)


type alias Model =
    { bookReference : ActionResult BookReference
    }


initialModel : Model
initialModel =
    { bookReference = Loading
    }


type alias BookReference =
    { shortUuid : String
    , content : String
    , bookChapter : String
    }


bookReferenceDecoder : Decoder BookReference
bookReferenceDecoder =
    Decode.succeed BookReference
        |> required "shortUuid" Decode.string
        |> required "content" Decode.string
        |> required "bookChapter" Decode.string
