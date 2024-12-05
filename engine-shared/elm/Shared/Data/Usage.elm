module Shared.Data.Usage exposing (Usage, UsageValue, decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Usage =
    { activeUsers : UsageValue
    , branches : UsageValue
    , documents : UsageValue
    , documentTemplateDrafts : UsageValue
    , documentTemplates : UsageValue
    , knowledgeModels : UsageValue
    , locales : UsageValue
    , questionnaires : UsageValue
    , storage : UsageValue
    , users : UsageValue
    }


type alias UsageValue =
    { max : Int
    , current : Int
    }


decoder : Decoder Usage
decoder =
    D.succeed Usage
        |> D.required "activeUsers" decodeUsageValue
        |> D.required "branches" decodeUsageValue
        |> D.required "documents" decodeUsageValue
        |> D.required "documentTemplateDrafts" decodeUsageValue
        |> D.required "documentTemplates" decodeUsageValue
        |> D.required "knowledgeModels" decodeUsageValue
        |> D.required "locales" decodeUsageValue
        |> D.required "questionnaires" decodeUsageValue
        |> D.required "storage" decodeUsageValue
        |> D.required "users" decodeUsageValue


decodeUsageValue : Decoder UsageValue
decodeUsageValue =
    D.succeed UsageValue
        |> D.required "max" D.int
        |> D.required "current" D.int
