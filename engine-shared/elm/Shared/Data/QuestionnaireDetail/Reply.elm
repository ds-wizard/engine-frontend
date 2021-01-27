module Shared.Data.QuestionnaireDetail.Reply exposing (..)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Shared.Data.QuestionnaireDetail.Reply.ReplyValue as ReplyValue exposing (ReplyValue)
import Shared.Data.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Time


type alias Reply =
    { value : ReplyValue
    , createdAt : Time.Posix
    , createdBy : Maybe UserSuggestion
    }


decoder : Decoder Reply
decoder =
    D.succeed Reply
        |> D.required "value" ReplyValue.decoder
        |> D.required "createdAt" D.datetime
        |> D.required "createdBy" (D.maybe UserSuggestion.decoder)
