module Wizard.Api.Models.KnowledgeModel.Question.FileQuestionData exposing
    ( FileQuestionData
    , decoder
    , encodeValues
    , equalContent
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


equalContent : FileQuestionData -> FileQuestionData -> Bool
equalContent data1 data2 =
    (data1.maxSize == data2.maxSize)
        && (data1.fileTypes == data2.fileTypes)
