module Shared.Data.KnowledgeModel.Expert exposing (Expert, decoder, new)

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Expert =
    { uuid : String
    , name : String
    , email : String
    , annotations : Dict String String
    }


new : String -> Expert
new uuid =
    { uuid = uuid
    , name = "New expert"
    , email = "expert@example.com"
    , annotations = Dict.empty
    }


decoder : Decoder Expert
decoder =
    D.succeed Expert
        |> D.required "uuid" D.string
        |> D.required "name" D.string
        |> D.required "email" D.string
        |> D.required "annotations" (D.dict D.string)
