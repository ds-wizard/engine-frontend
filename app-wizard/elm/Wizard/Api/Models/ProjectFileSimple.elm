module Wizard.Api.Models.ProjectFileSimple exposing
    ( ProjectFileSimple
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias ProjectFileSimple =
    { uuid : Uuid
    , contentType : String
    , fileName : String
    , fileSize : Int
    }


decoder : Decoder ProjectFileSimple
decoder =
    D.succeed ProjectFileSimple
        |> D.required "uuid" Uuid.decoder
        |> D.required "contentType" D.string
        |> D.required "fileName" D.string
        |> D.required "fileSize" D.int
