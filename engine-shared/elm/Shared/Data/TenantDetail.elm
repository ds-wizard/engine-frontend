module Shared.Data.TenantDetail exposing
    ( TenantDetail
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Shared.Data.Usage as Usage exposing (Usage)
import Shared.Data.User as User exposing (User)
import Time
import Uuid exposing (Uuid)


type alias TenantDetail =
    { uuid : Uuid
    , tenantId : String
    , name : String
    , enabled : Bool
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    , clientUrl : String
    , serverUrl : String
    , users : List User
    , usage : Usage
    , primaryColor : Maybe String
    , logoUrl : Maybe String
    }


decoder : Decoder TenantDetail
decoder =
    D.succeed TenantDetail
        |> D.required "uuid" Uuid.decoder
        |> D.required "tenantId" D.string
        |> D.required "name" D.string
        |> D.required "enabled" D.bool
        |> D.required "createdAt" D.datetime
        |> D.required "updatedAt" D.datetime
        |> D.required "clientUrl" D.string
        |> D.required "serverUrl" D.string
        |> D.required "users" (D.list User.decoder)
        |> D.required "usage" Usage.decoder
        |> D.required "primaryColor" (D.maybe D.string)
        |> D.required "logoUrl" (D.maybe D.string)
