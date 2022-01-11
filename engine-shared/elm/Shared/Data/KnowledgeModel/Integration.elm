module Shared.Data.KnowledgeModel.Integration exposing
    ( Integration
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.Integration.RequestHeader as RequestHeader exposing (RequestHeader)


type alias Integration =
    { uuid : String
    , id : String
    , name : String
    , props : List String
    , logo : String
    , requestMethod : String
    , requestUrl : String
    , requestHeaders : List RequestHeader
    , requestBody : String
    , responseListField : String
    , responseItemId : String
    , responseItemTemplate : String
    , responseItemUrl : String
    , annotations : List Annotation
    }


decoder : Decoder Integration
decoder =
    D.succeed Integration
        |> D.required "uuid" D.string
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "props" (D.list D.string)
        |> D.required "logo" D.string
        |> D.required "requestMethod" D.string
        |> D.required "requestUrl" D.string
        |> D.required "requestHeaders" (D.list RequestHeader.decoder)
        |> D.required "requestBody" D.string
        |> D.required "responseListField" D.string
        |> D.required "responseItemId" D.string
        |> D.required "responseItemTemplate" D.string
        |> D.required "responseItemUrl" D.string
        |> D.required "annotations" (D.list Annotation.decoder)
