module Wizard.Api.Models.ProjectDetail.ProjectEvent.ClearReplyData exposing
    ( ClearReplyData
    , decoder
    , encode
    )

import Common.Api.Models.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Time
import Uuid exposing (Uuid)


type alias ClearReplyData =
    { uuid : Uuid
    , path : String
    , createdAt : Time.Posix
    , createdBy : Maybe UserSuggestion
    }


encode : ClearReplyData -> E.Value
encode data =
    E.object
        [ ( "type", E.string "ClearReplyEvent" )
        , ( "uuid", Uuid.encode data.uuid )
        , ( "path", E.string data.path )
        ]


decoder : Decoder ClearReplyData
decoder =
    D.succeed ClearReplyData
        |> D.required "uuid" Uuid.decoder
        |> D.required "path" D.string
        |> D.required "createdAt" D.datetime
        |> D.required "createdBy" (D.maybe UserSuggestion.decoder)
