module Wizard.Api.Models.KnowledgeModel.Question.OptionsQuestionData exposing
    ( OptionsQuestionData
    , decoder
    , encodeValues
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias OptionsQuestionData =
    { answerUuids : List String
    }


decoder : Decoder OptionsQuestionData
decoder =
    D.succeed OptionsQuestionData
        |> D.required "answerUuids" (D.list D.string)


encodeValues : OptionsQuestionData -> List ( String, E.Value )
encodeValues optionsData =
    [ ( "answerUuids", E.list E.string optionsData.answerUuids )
    ]
