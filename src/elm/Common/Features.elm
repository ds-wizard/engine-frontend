module Common.Features exposing (Features, featuresDecoder, initFeatures)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)


type alias Features =
    { feedback : Bool
    , registration : Bool
    , publicQuestionnaire : Bool
    }


initFeatures : Features
initFeatures =
    { feedback = True
    , registration = True
    , publicQuestionnaire = True
    }


featuresDecoder : Decoder Features
featuresDecoder =
    Decode.succeed Features
        |> required "feedback" Decode.bool
        |> required "registration" Decode.bool
        |> required "publicQuestionnaire" Decode.bool
