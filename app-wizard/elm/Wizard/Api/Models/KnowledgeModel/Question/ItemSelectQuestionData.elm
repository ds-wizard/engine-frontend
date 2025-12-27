module Wizard.Api.Models.KnowledgeModel.Question.ItemSelectQuestionData exposing
    ( ItemSelectQuestionData
    , decoder
    , encodeValues
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E


type alias ItemSelectQuestionData =
    { listQuestionUuid : Maybe String
    }


decoder : Decoder ItemSelectQuestionData
decoder =
    D.succeed ItemSelectQuestionData
        |> D.required "listQuestionUuid" (D.maybe D.string)


encodeValues : ItemSelectQuestionData -> List ( String, E.Value )
encodeValues itemSelectData =
    [ ( "listQuestionUuid", E.maybe E.string itemSelectData.listQuestionUuid )
    ]
