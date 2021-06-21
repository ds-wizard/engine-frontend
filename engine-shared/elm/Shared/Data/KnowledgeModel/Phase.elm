module Shared.Data.KnowledgeModel.Phase exposing (Phase, decoder, new)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid


type alias Phase =
    { uuid : String
    , title : String
    , description : Maybe String
    }


decoder : Decoder Phase
decoder =
    D.succeed Phase
        |> D.optional "uuid" D.string (Uuid.toString Uuid.nil)
        |> D.required "title" D.string
        |> D.required "description" (D.maybe D.string)


new : String -> Phase
new uuid =
    { uuid = uuid
    , title = "New Phase"
    , description = Nothing
    }
