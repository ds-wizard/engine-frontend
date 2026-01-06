module Wizard.Api.Models.ProjectSuggestion exposing
    ( ProjectSuggestion
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias ProjectSuggestion =
    { uuid : Uuid
    , name : String
    , description : Maybe String
    }


decoder : Decoder ProjectSuggestion
decoder =
    D.succeed ProjectSuggestion
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "description" (D.maybe D.string)
