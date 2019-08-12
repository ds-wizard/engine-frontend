module KMEditor.Common.KnowledgeModel.Question.ListQuestionData exposing
    ( ListQuestionData
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias ListQuestionData =
    { itemTemplateTitle : String
    , itemTemplateQuestionUuids : List String
    }


decoder : Decoder ListQuestionData
decoder =
    D.succeed ListQuestionData
        |> D.required "itemTemplateTitle" D.string
        |> D.required "itemTemplateQuestionUuids" (D.list D.string)
