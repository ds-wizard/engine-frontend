module Wizard.Api.Models.ProjectDetailWrapper exposing (ProjectDetailWrapper, decoder)

import Json.Decode as D exposing (Decoder)
import Wizard.Api.Models.ProjectCommon as ProjectCommon exposing (ProjectCommon)


type alias ProjectDetailWrapper a =
    { common : ProjectCommon
    , data : a
    }


decoder : Decoder a -> Decoder (ProjectDetailWrapper a)
decoder dataDecoder =
    D.map2 ProjectDetailWrapper
        ProjectCommon.decoder
        dataDecoder
