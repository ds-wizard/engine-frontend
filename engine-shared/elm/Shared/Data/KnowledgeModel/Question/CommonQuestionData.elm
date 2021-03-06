module Shared.Data.KnowledgeModel.Question.CommonQuestionData exposing
    ( CommonQuestionData
    , decoder
    , new
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias CommonQuestionData =
    { uuid : String
    , title : String
    , text : Maybe String
    , requiredPhaseUuid : Maybe String
    , tagUuids : List String
    , referenceUuids : List String
    , expertUuids : List String
    }


decoder : Decoder CommonQuestionData
decoder =
    D.succeed CommonQuestionData
        |> D.required "uuid" D.string
        |> D.required "title" D.string
        |> D.required "text" (D.nullable D.string)
        |> D.required "requiredPhaseUuid" (D.nullable D.string)
        |> D.required "tagUuids" (D.list D.string)
        |> D.required "referenceUuids" (D.list D.string)
        |> D.required "expertUuids" (D.list D.string)


new : String -> CommonQuestionData
new uuid =
    { uuid = uuid
    , title = "New question"
    , text = Nothing
    , requiredPhaseUuid = Nothing
    , tagUuids = []
    , referenceUuids = []
    , expertUuids = []
    }
