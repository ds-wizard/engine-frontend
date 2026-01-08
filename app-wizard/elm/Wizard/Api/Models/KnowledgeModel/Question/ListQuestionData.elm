module Wizard.Api.Models.KnowledgeModel.Question.ListQuestionData exposing
    ( ListQuestionData
    , decoder
    , encodeValues
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias ListQuestionData =
    { itemTemplateQuestionUuids : List String
    }


decoder : Decoder ListQuestionData
decoder =
    D.succeed ListQuestionData
        |> D.required "itemTemplateQuestionUuids" (D.list D.string)


encodeValues : ListQuestionData -> List ( String, E.Value )
encodeValues listData =
    [ ( "itemTemplateQuestionUuids", E.list E.string listData.itemTemplateQuestionUuids ) ]
