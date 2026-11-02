module Common.Api.Models.ProjectDetailWrapper exposing (ProjectDetailWrapper, decoder)

import Common.Api.Models.ProjectCommon as ProjectCommon exposing (ProjectCommon)
import Json.Decode as D exposing (Decoder)


type alias ProjectDetailWrapper a =
    { common : ProjectCommon
    , data : a
    }


decoder : Decoder a -> Decoder (ProjectDetailWrapper a)
decoder dataDecoder =
    D.map2 ProjectDetailWrapper
        ProjectCommon.decoder
        dataDecoder
