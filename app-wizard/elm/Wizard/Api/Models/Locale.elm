module Wizard.Api.Models.Locale exposing
    ( Locale
    , decoder
    , isOutdated
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Version exposing (Version)
import Wizard.Api.Models.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)


type alias Locale =
    { id : String
    , localeId : String
    , name : String
    , description : String
    , code : String
    , organizationId : String
    , version : Version
    , defaultLocale : Bool
    , enabled : Bool
    , createdAt : Time.Posix
    , remoteLatestVersion : Maybe Version
    , organization : Maybe OrganizationInfo
    }


decoder : Decoder Locale
decoder =
    D.succeed Locale
        |> D.required "id" D.string
        |> D.required "localeId" D.string
        |> D.required "name" D.string
        |> D.required "description" D.string
        |> D.required "code" D.string
        |> D.required "organizationId" D.string
        |> D.required "version" Version.decoder
        |> D.required "defaultLocale" D.bool
        |> D.required "enabled" D.bool
        |> D.required "createdAt" D.datetime
        |> D.required "remoteLatestVersion" (D.maybe Version.decoder)
        |> D.optional "organization" (D.maybe OrganizationInfo.decoder) Nothing


isOutdated : { a | remoteLatestVersion : Maybe Version, version : Version } -> Bool
isOutdated template =
    case template.remoteLatestVersion of
        Just remoteLatestVersion ->
            Version.greaterThan template.version remoteLatestVersion

        Nothing ->
            False
