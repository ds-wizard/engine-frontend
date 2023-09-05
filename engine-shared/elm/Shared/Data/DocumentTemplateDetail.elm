module Shared.Data.DocumentTemplateDetail exposing
    ( DocumentTemplateDetail
    , decoder
    , encode
    , isLatestVersion
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.DocumentTemplate.DocumentTemplateAllowedPackage as DocumentTemplateAllowedPackage exposing (DocumentTemplateAllowedPackage)
import Shared.Data.DocumentTemplate.DocumentTemplateFormat as DocumentTemplateFormat exposing (DocumentTemplateFormat)
import Shared.Data.DocumentTemplate.DocumentTemplatePackage as DocumentTemplatePackage exposing (DocumentTemplatePackage)
import Shared.Data.DocumentTemplate.DocumentTemplatePhase as DocumentTemplatePhase exposing (DocumentTemplatePhase)
import Shared.Data.DocumentTemplate.DocumentTemplateState as DocumentTemplateState exposing (DocumentTemplateState)
import Shared.Data.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import Time
import Version exposing (Version)


type alias DocumentTemplateDetail =
    { allowedPackages : List DocumentTemplateAllowedPackage
    , createdAt : Time.Posix
    , description : String
    , formats : List DocumentTemplateFormat
    , id : String
    , license : String
    , metamodelVersion : Int
    , name : String
    , organization : Maybe OrganizationInfo
    , organizationId : String
    , phase : DocumentTemplatePhase
    , readme : String
    , registryLink : Maybe String
    , remoteLatestVersion : Maybe Version
    , state : DocumentTemplateState
    , templateId : String
    , usablePackages : List DocumentTemplatePackage
    , version : Version
    , versions : List Version
    , nonEditable : Bool
    }


decoder : Decoder DocumentTemplateDetail
decoder =
    D.succeed DocumentTemplateDetail
        |> D.required "allowedPackages" (D.list DocumentTemplateAllowedPackage.decoder)
        |> D.required "createdAt" D.datetime
        |> D.required "description" D.string
        |> D.required "formats" (D.list DocumentTemplateFormat.decoder)
        |> D.required "id" D.string
        |> D.required "license" D.string
        |> D.required "metamodelVersion" D.int
        |> D.required "name" D.string
        |> D.optional "organization" (D.maybe OrganizationInfo.decoder) Nothing
        |> D.required "organizationId" D.string
        |> D.required "phase" DocumentTemplatePhase.decoder
        |> D.required "readme" D.string
        |> D.required "registryLink" (D.maybe D.string)
        |> D.required "remoteLatestVersion" (D.maybe Version.decoder)
        |> D.required "state" DocumentTemplateState.decoder
        |> D.required "templateId" D.string
        |> D.required "usablePackages" (D.list DocumentTemplatePackage.decoder)
        |> D.required "version" Version.decoder
        |> D.required "versions" (D.list Version.decoder)
        |> D.required "nonEditable" D.bool


encode : { a | phase : DocumentTemplatePhase } -> E.Value
encode documentTemplate =
    E.object
        [ ( "phase", DocumentTemplatePhase.encode documentTemplate.phase ) ]


isLatestVersion : DocumentTemplateDetail -> Bool
isLatestVersion documentTemplate =
    List.isEmpty <| List.filter (Version.greaterThan documentTemplate.version) documentTemplate.versions
