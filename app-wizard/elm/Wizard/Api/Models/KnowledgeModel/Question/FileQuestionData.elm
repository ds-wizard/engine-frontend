module Wizard.Api.Models.KnowledgeModel.Question.FileQuestionData exposing
    ( FileQuestionData
    , decoder
    , encodeValues
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E


type alias FileQuestionData =
    { maxSize : Maybe Int
    , fileTypes : Maybe String
    }


decoder : Decoder FileQuestionData
decoder =
    D.succeed FileQuestionData
        |> D.required "maxSize" (D.maybe D.int)
        |> D.required "fileTypes" (D.maybe D.string)


encodeValues : FileQuestionData -> List ( String, E.Value )
encodeValues fileData =
    [ ( "maxSize", E.maybe E.int fileData.maxSize )
    , ( "fileTypes", E.maybe E.string fileData.fileTypes )
    ]
