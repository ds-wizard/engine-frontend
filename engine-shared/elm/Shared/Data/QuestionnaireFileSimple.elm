module Shared.Data.QuestionnaireFileSimple exposing
    ( QuestionnaireFileSimple
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias QuestionnaireFileSimple =
    { uuid : Uuid
    , contentType : String
    , fileName : String
    , fileSize : Int
    }


decoder : Decoder QuestionnaireFileSimple
decoder =
    D.succeed QuestionnaireFileSimple
        |> D.required "uuid" Uuid.decoder
        |> D.required "contentType" D.string
        |> D.required "fileName" D.string
        |> D.required "fileSize" D.int
