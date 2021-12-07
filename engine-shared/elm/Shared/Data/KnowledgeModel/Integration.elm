module Shared.Data.KnowledgeModel.Integration exposing
    ( Integration
    , decoder
    , new
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Integration =
    { uuid : String
    , id : String
    , name : String
    , props : List String
    , logo : String
    , requestMethod : String
    , requestUrl : String
    , requestHeaders : Dict String String
    , requestBody : String
    , responseListField : String
    , responseItemId : String
    , responseItemTemplate : String
    , responseItemUrl : String
    , annotations : Dict String String
    }


new : String -> Integration
new uuid =
    { uuid = uuid
    , id = ""
    , name = "New Integration"
    , props = []
    , logo = ""
    , requestMethod = "GET"
    , requestUrl = "/"
    , requestHeaders = Dict.empty
    , requestBody = ""
    , responseListField = ""
    , responseItemId = "{{item.id}}"
    , responseItemTemplate = "{{item.name}}"
    , responseItemUrl = ""
    , annotations = Dict.empty
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
        |> D.required "requestHeaders" (D.dict D.string)
        |> D.required "requestBody" D.string
        |> D.required "responseListField" D.string
        |> D.required "responseItemId" D.string
        |> D.required "responseItemTemplate" D.string
        |> D.required "responseItemUrl" D.string
        |> D.required "annotations" (D.dict D.string)
