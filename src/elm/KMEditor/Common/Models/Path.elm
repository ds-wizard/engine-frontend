module KMEditor.Common.Models.Path exposing (Path, PathNode(..), createEncodedPathNode, encodePathNode, getNodeUuid, getParentUuid, pathDecoder, pathNodeDecoder, pathNodeDecoderByType, pathNodeUuidDecoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (..)
import List.Extra as List


type PathNode
    = KMPathNode String
    | ChapterPathNode String
    | QuestionPathNode String
    | AnswerPathNode String


type alias Path =
    List PathNode



{- Encoders -}


encodePathNode : PathNode -> Encode.Value
encodePathNode node =
    case node of
        KMPathNode uuid ->
            createEncodedPathNode "km" uuid

        ChapterPathNode uuid ->
            createEncodedPathNode "chapter" uuid

        QuestionPathNode uuid ->
            createEncodedPathNode "question" uuid

        AnswerPathNode uuid ->
            createEncodedPathNode "answer" uuid


createEncodedPathNode : String -> String -> Encode.Value
createEncodedPathNode pathNodeType uuid =
    Encode.object
        [ ( "type", Encode.string pathNodeType )
        , ( "uuid", Encode.string uuid )
        ]



{- Decoders -}


pathDecoder : Decoder Path
pathDecoder =
    Decode.list pathNodeDecoder


pathNodeDecoder : Decoder PathNode
pathNodeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen pathNodeDecoderByType


pathNodeDecoderByType : String -> Decoder PathNode
pathNodeDecoderByType pathNodeType =
    case pathNodeType of
        "km" ->
            Decode.map KMPathNode pathNodeUuidDecoder

        "chapter" ->
            Decode.map ChapterPathNode pathNodeUuidDecoder

        "question" ->
            Decode.map QuestionPathNode pathNodeUuidDecoder

        "answer" ->
            Decode.map AnswerPathNode pathNodeUuidDecoder

        _ ->
            Decode.fail <| "Unknown path node type: " ++ pathNodeType


pathNodeUuidDecoder : Decoder String
pathNodeUuidDecoder =
    Decode.field "uuid" Decode.string



{- Utils -}


getParentUuid : Path -> Maybe String
getParentUuid path =
    List.last path |> Maybe.map getNodeUuid


getNodeUuid : PathNode -> String
getNodeUuid pathNode =
    case pathNode of
        KMPathNode uuid ->
            uuid

        ChapterPathNode uuid ->
            uuid

        QuestionPathNode uuid ->
            uuid

        AnswerPathNode uuid ->
            uuid
