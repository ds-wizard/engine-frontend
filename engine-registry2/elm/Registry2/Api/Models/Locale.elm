module Registry2.Api.Models.Locale exposing
    ( Locale
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Registry2.Api.Models.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import Time
import Version exposing (Version)


type alias Locale =
    { id : String
    , name : String
    , code : String
    , localeId : String
    , version : Version
    , description : String
    , organization : OrganizationInfo
    , createdAt : Time.Posix
    }


decoder : Decoder Locale
decoder =
    D.succeed Locale
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "code" D.string
        |> D.required "localeId" D.string
        |> D.required "version" Version.decoder
        |> D.required "description" D.string
        |> D.required "organization" OrganizationInfo.decoder
        |> D.required "createdAt" D.datetime
