module Shared.Data.LocaleDetail exposing
    ( LocaleDetail
    , decoder
    , encode
    , isLatestVersion
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.Locale.LocaleState as LocaleState exposing (LocaleState)
import Shared.Data.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import Time
import Version exposing (Version)


type alias LocaleDetail =
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
    , state : LocaleState
    , remoteLatestVersion : Maybe Version
    , organization : Maybe OrganizationInfo
    , license : String
    , readme : String
    , recommendedAppVersion : Version
    , versions : List Version
    }


decoder : Decoder LocaleDetail
decoder =
    D.succeed LocaleDetail
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
        |> D.required "state" LocaleState.decoder
        |> D.required "remoteLatestVersion" (D.maybe Version.decoder)
        |> D.optional "organization" (D.maybe OrganizationInfo.decoder) Nothing
        |> D.required "license" D.string
        |> D.required "readme" D.string
        |> D.required "recommendedAppVersion" Version.decoder
        |> D.required "versions" (D.list Version.decoder)


isLatestVersion : LocaleDetail -> Bool
isLatestVersion locale =
    List.isEmpty <| List.filter (Version.greaterThan locale.version) locale.versions


encode : { a | enabled : Bool, defaultLocale : Bool } -> E.Value
encode locale =
    E.object
        [ ( "enabled", E.bool locale.enabled )
        , ( "defaultLocale", E.bool locale.defaultLocale )
        ]
