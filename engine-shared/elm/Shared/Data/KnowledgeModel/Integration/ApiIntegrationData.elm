module Shared.Data.KnowledgeModel.Integration.ApiIntegrationData exposing
    ( ApiIntegrationData
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.KnowledgeModel.Integration.RequestHeader as RequestHeader exposing (RequestHeader)


type alias ApiIntegrationData =
    { requestMethod : String
    , requestUrl : String
    , requestHeaders : List RequestHeader
    , requestBody : String
    , requestEmptySearch : Bool
    , responseListField : String
    , responseItemId : String
    , responseItemTemplate : String
    }


decoder : Decoder ApiIntegrationData
decoder =
    D.succeed ApiIntegrationData
        |> D.required "requestMethod" D.string
        |> D.required "requestUrl" D.string
        |> D.required "requestHeaders" (D.list RequestHeader.decoder)
        |> D.required "requestBody" D.string
        |> D.required "requestEmptySearch" D.bool
        |> D.required "responseListField" D.string
        |> D.required "responseItemId" D.string
        |> D.required "responseItemTemplate" D.string
