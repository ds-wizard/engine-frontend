module Shared.Data.KnowledgeModel.Question.MultiChoiceQuestionData exposing
    ( MultiChoiceQuestionData
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias MultiChoiceQuestionData =
    { choiceUuids : List String }


decoder : Decoder MultiChoiceQuestionData
decoder =
    D.succeed MultiChoiceQuestionData
        |> D.required "choiceUuids" (D.list D.string)
