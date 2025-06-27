module Wizard.Api.Models.KnowledgeModel.Question.ListQuestionData exposing
    ( ListQuestionData
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias ListQuestionData =
    { itemTemplateQuestionUuids : List String
    }


decoder : Decoder ListQuestionData
decoder =
    D.succeed ListQuestionData
        |> D.required "itemTemplateQuestionUuids" (D.list D.string)
