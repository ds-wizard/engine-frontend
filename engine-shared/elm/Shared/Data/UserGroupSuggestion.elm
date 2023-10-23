module Shared.Data.UserGroupSuggestion exposing (UserGroupSuggestion, decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias UserGroupSuggestion =
    { uuid : Uuid
    , name : String
    , description : Maybe String
    , private : Bool
    }


decoder : Decoder UserGroupSuggestion
decoder =
    D.succeed UserGroupSuggestion
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "description" (D.maybe D.string)
        |> D.required "private" D.bool
