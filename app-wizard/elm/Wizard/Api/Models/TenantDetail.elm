module Wizard.Api.Models.TenantDetail exposing
    ( TenantDetail
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Uuid exposing (Uuid)
import Wizard.Api.Models.TenantState as TenantState exposing (TenantState)
import Wizard.Api.Models.Usage as Usage exposing (Usage)
import Wizard.Api.Models.User as User exposing (User)


type alias TenantDetail =
    { uuid : Uuid
    , tenantId : String
    , name : String
    , enabled : Bool
    , state : TenantState
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
        |> D.required "state" TenantState.decoder
        |> D.required "createdAt" D.datetime
        |> D.required "updatedAt" D.datetime
        |> D.required "clientUrl" D.string
        |> D.required "serverUrl" D.string
        |> D.required "users" (D.list User.decoder)
        |> D.required "usage" Usage.decoder
        |> D.required "primaryColor" (D.maybe D.string)
        |> D.required "logoUrl" (D.maybe D.string)
