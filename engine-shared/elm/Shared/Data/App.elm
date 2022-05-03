module Shared.Data.App exposing
    ( App
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Uuid exposing (Uuid)


type alias App =
    { uuid : Uuid
    , appId : String
    , name : String
    , enabled : Bool
    , logoUrl : Maybe String
    , primaryColor : Maybe String
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    , clientUrl : String
    }


decoder : Decoder App
decoder =
    D.succeed App
        |> D.required "uuid" Uuid.decoder
        |> D.required "appId" D.string
        |> D.required "name" D.string
        |> D.required "enabled" D.bool
        |> D.required "logoUrl" (D.maybe D.string)
        |> D.required "primaryColor" (D.maybe D.string)
        |> D.required "createdAt" D.datetime
        |> D.required "updatedAt" D.datetime
        |> D.required "clientUrl" D.string
