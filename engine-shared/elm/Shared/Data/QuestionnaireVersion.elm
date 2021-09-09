module Shared.Data.QuestionnaireVersion exposing
    ( QuestionnaireVersion
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Shared.Data.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Time
import Uuid exposing (Uuid)


type alias QuestionnaireVersion =
    { uuid : Uuid
    , name : String
    , description : Maybe String
    , eventUuid : Uuid
    , createdAt : Time.Posix
    , createdBy : UserSuggestion
    }


decoder : Decoder QuestionnaireVersion
decoder =
    D.succeed QuestionnaireVersion
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "description" (D.maybe D.string)
        |> D.required "eventUuid" Uuid.decoder
        |> D.required "createdAt" D.datetime
        |> D.required "createdBy" UserSuggestion.decoder
