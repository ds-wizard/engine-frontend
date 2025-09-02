module Wizard.Api.Models.KnowledgeModel.Integration.ApiLegacyIntegrationData exposing
    ( ApiLegacyIntegrationData
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Api.Models.KnowledgeModel.Integration.KeyValuePair as KeyValuePair exposing (KeyValuePair)


type alias ApiLegacyIntegrationData =
    { requestMethod : String
    , requestUrl : String
    , requestHeaders : List KeyValuePair
    , requestBody : String
    , requestEmptySearch : Bool
    , responseListField : Maybe String
    , responseItemId : Maybe String
    , responseItemTemplate : String
    }


decoder : Decoder ApiLegacyIntegrationData
decoder =
    D.succeed ApiLegacyIntegrationData
        |> D.required "requestMethod" D.string
        |> D.required "requestUrl" D.string
        |> D.required "requestHeaders" (D.list KeyValuePair.decoder)
        |> D.required "requestBody" D.string
        |> D.required "requestEmptySearch" D.bool
        |> D.required "responseListField" (D.maybe D.string)
        |> D.required "responseItemId" (D.maybe D.string)
        |> D.required "responseItemTemplate" D.string
