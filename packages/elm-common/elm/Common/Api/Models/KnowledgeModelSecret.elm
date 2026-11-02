module Common.Api.Models.KnowledgeModelSecret exposing
    ( KnowledgeModelSecret
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Uuid exposing (Uuid)


type alias KnowledgeModelSecret =
    { createdAt : Time.Posix
    , name : String
    , updatedAt : Time.Posix
    , uuid : Uuid
    , workspaceUuid : Maybe Uuid
    }


decoder : Decoder KnowledgeModelSecret
decoder =
    D.succeed KnowledgeModelSecret
        |> D.required "createdAt" D.datetime
        |> D.required "name" D.string
        |> D.required "updatedAt" D.datetime
        |> D.required "uuid" Uuid.decoder
        |> D.optional "workspaceUuid" (D.nullable Uuid.decoder) Nothing
