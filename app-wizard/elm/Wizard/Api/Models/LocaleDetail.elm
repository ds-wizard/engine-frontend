module Wizard.Api.Models.LocaleDetail exposing
    ( LocaleDetail
    , decoder
    , encode
    , isLatestVersion
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Time
import Uuid exposing (Uuid)
import Version exposing (Version)
import Wizard.Api.Models.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import Wizard.Api.Models.VersionUuid as VersionUuid exposing (VersionUuid)


type alias LocaleDetail =
    { uuid : Uuid
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
    , registryLink : Maybe String
    , license : String
    , readme : String
    , recommendedAppVersion : Version
    , versions : List VersionUuid
    }


decoder : Decoder LocaleDetail
decoder =
    D.succeed LocaleDetail
        |> D.required "uuid" Uuid.decoder
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
        |> D.required "registryLink" (D.maybe D.string)
        |> D.required "license" D.string
        |> D.required "readme" D.string
        |> D.required "recommendedAppVersion" Version.decoder
        |> D.required "versions" (D.list VersionUuid.decoder)


isLatestVersion : LocaleDetail -> Bool
isLatestVersion locale =
    locale.versions
        |> List.any (Version.greaterThan locale.version << .version)
        |> not


encode : { a | enabled : Bool, defaultLocale : Bool } -> E.Value
encode locale =
    E.object
        [ ( "enabled", E.bool locale.enabled )
        , ( "defaultLocale", E.bool locale.defaultLocale )
        ]
