module Wizard.Api.Models.KnowledgeModel.Question.MultiChoiceQuestionData exposing
    ( MultiChoiceQuestionData
    , decoder
    , encodeValues
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias MultiChoiceQuestionData =
    { choiceUuids : List String }


decoder : Decoder MultiChoiceQuestionData
decoder =
    D.succeed MultiChoiceQuestionData
        |> D.required "choiceUuids" (D.list D.string)


encodeValues : MultiChoiceQuestionData -> List ( String, E.Value )
encodeValues multiChoiceData =
    [ ( "choiceUuids", E.list E.string multiChoiceData.choiceUuids )
    ]
