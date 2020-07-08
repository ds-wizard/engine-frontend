module Shared.Data.TemplateDetail exposing (TemplateDetail, decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Shared.Data.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import Shared.Data.Template.TemplateFormat as TemplateFormat exposing (TemplateFormat)
import Shared.Data.Template.TemplatePackage as TemplatePackage exposing (TemplatePackage)
import Shared.Data.Template.TemplateState as TemplateState exposing (TemplateState)
import Time
import Version exposing (Version)


type alias TemplateDetail =
    { createdAt : Time.Posix
    , description : String
    , formats : List TemplateFormat
    , id : String
    , license : String
    , metamodelVersion : Int
    , name : String
    , organization : Maybe OrganizationInfo
    , organizationId : String
    , readme : String
    , recommendedPackageId : Maybe String
    , registryLink : Maybe String
    , remoteLatestVersion : Maybe Version
    , state : TemplateState
    , templateId : String
    , usablePackages : List TemplatePackage
    , version : Version
    , versions : List Version
    }


decoder : Decoder TemplateDetail
decoder =
    D.succeed TemplateDetail
        |> D.required "createdAt" D.datetime
        |> D.required "description" D.string
        |> D.required "formats" (D.list TemplateFormat.decoder)
        |> D.required "id" D.string
        |> D.required "license" D.string
        |> D.required "metamodelVersion" D.int
        |> D.required "name" D.string
        |> D.optional "organization" (D.maybe OrganizationInfo.decoder) Nothing
        |> D.required "organizationId" D.string
        |> D.required "readme" D.string
        |> D.required "recommendedPackageId" (D.maybe D.string)
        |> D.required "registryLink" (D.maybe D.string)
        |> D.required "remoteLatestVersion" (D.maybe Version.decoder)
        |> D.required "state" TemplateState.decoder
        |> D.required "templateId" D.string
        |> D.required "usablePackages" (D.list TemplatePackage.decoder)
        |> D.required "version" Version.decoder
        |> D.required "versions" (D.list Version.decoder)
