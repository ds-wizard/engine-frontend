module Wizard.Api.Models.Usage exposing (Usage, decoder)

import Common.Api.Models.UsageValue as UsageValue exposing (UsageValue)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Usage =
    { activeUsers : UsageValue
    , documents : UsageValue
    , documentTemplateDrafts : UsageValue
    , documentTemplates : UsageValue
    , knowledgeModelEditors : UsageValue
    , knowledgeModels : UsageValue
    , locales : UsageValue
    , projects : UsageValue
    , storage : UsageValue
    , users : UsageValue
    }


decoder : Decoder Usage
decoder =
    D.succeed Usage
        |> D.required "activeUsers" UsageValue.decoder
        |> D.required "documents" UsageValue.decoder
        |> D.required "documentTemplateDrafts" UsageValue.decoder
        |> D.required "documentTemplates" UsageValue.decoder
        |> D.required "knowledgeModelEditors" UsageValue.decoder
        |> D.required "knowledgeModels" UsageValue.decoder
        |> D.required "locales" UsageValue.decoder
        |> D.required "projects" UsageValue.decoder
        |> D.required "storage" UsageValue.decoder
        |> D.required "users" UsageValue.decoder
