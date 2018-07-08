module Public.BookReference.Models exposing (..)

import Common.Types exposing (ActionResult(Loading))
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)


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
    decode BookReference
        |> required "shortUuid" Decode.string
        |> required "content" Decode.string
        |> required "bookChapter" Decode.string
