module Shared.Data.Tenant exposing
    ( Tenant
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Uuid exposing (Uuid)


type alias Tenant =
    { uuid : Uuid
    , tenantId : String
    , name : String
    , enabled : Bool
    , logoUrl : Maybe String
    , primaryColor : Maybe String
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    , clientUrl : String
    }


decoder : Decoder Tenant
decoder =
    D.succeed Tenant
        |> D.required "uuid" Uuid.decoder
        |> D.required "tenantId" D.string
        |> D.required "name" D.string
        |> D.required "enabled" D.bool
        |> D.required "logoUrl" (D.maybe D.string)
        |> D.required "primaryColor" (D.maybe D.string)
        |> D.required "createdAt" D.datetime
        |> D.required "updatedAt" D.datetime
        |> D.required "clientUrl" D.string
