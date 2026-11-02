module Common.Api.Models.KnowledgeModelPackage exposing
    ( KnowledgeModelPackage
    , decoder
    , dummy
    , isOutdated
    )

import Common.Api.Models.KnowledgeModelPackage.KnowledgeModelPackagePhase as KnowledgeModelPackagePhase exposing (KnowledgeModelPackagePhase)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Uuid exposing (Uuid)
import Version exposing (Version)


type alias KnowledgeModelPackage =
    { uuid : Uuid
    , name : String
    , id : String
    , version : Version
    , description : String
    , language : String
    , remoteLatestVersion : Maybe Version
    , phase : KnowledgeModelPackagePhase
    , createdAt : Time.Posix
    , nonEditable : Bool
    , public : Bool
    , workspaceUuid : Maybe Uuid
    }


decoder : Decoder KnowledgeModelPackage
decoder =
    D.succeed KnowledgeModelPackage
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "id" D.string
        |> D.required "version" Version.decoder
        |> D.required "description" D.string
        |> D.required "language" D.string
        |> D.required "remoteLatestVersion" (D.maybe Version.decoder)
        |> D.required "phase" KnowledgeModelPackagePhase.decoder
        |> D.required "createdAt" D.datetime
        |> D.required "nonEditable" D.bool
        |> D.required "public" D.bool
        |> D.optional "workspaceUuid" (D.nullable Uuid.decoder) Nothing


dummy : KnowledgeModelPackage
dummy =
    { uuid = Uuid.nil
    , name = ""
    , id = ""
    , version = Version.create 0 0 0
    , description = ""
    , language = ""
    , remoteLatestVersion = Nothing
    , phase = KnowledgeModelPackagePhase.Released
    , createdAt = Time.millisToPosix 0
    , nonEditable = True
    , public = False
    , workspaceUuid = Nothing
    }


isOutdated : { a | remoteLatestVersion : Maybe Version, version : Version } -> Bool
isOutdated kmPackage =
    case kmPackage.remoteLatestVersion of
        Just remoteLatestVersion ->
            Version.greaterThan kmPackage.version remoteLatestVersion

        Nothing ->
            False
