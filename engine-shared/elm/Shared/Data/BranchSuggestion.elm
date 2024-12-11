module Shared.Data.BranchSuggestion exposing
    ( BranchSuggestion
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias BranchSuggestion =
    { uuid : Uuid
    , name : String
    }


decoder : Decoder BranchSuggestion
decoder =
    D.succeed BranchSuggestion
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
