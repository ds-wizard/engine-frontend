module Wizard.Api.Models.QuestionnaireDetailWrapper exposing (QuestionnaireDetailWrapper, decoder)

import Json.Decode as D exposing (Decoder)
import Wizard.Api.Models.QuestionnaireCommon as QuestionnaireCommon exposing (QuestionnaireCommon)


type alias QuestionnaireDetailWrapper a =
    { common : QuestionnaireCommon
    , data : a
    }


decoder : Decoder a -> Decoder (QuestionnaireDetailWrapper a)
decoder dataDecoder =
    D.map2 QuestionnaireDetailWrapper
        QuestionnaireCommon.decoder
        dataDecoder
