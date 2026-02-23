module Registry.Api.Models.KnowledgeModelDetail exposing
    ( KnowledgeModelDetail
    , decoder
    , getId
    , otherVersionId
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Registry.Api.Models.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import Time
import Uuid exposing (Uuid)
import Version exposing (Version)


type alias KnowledgeModelDetail =
    { uuid : Uuid
    , name : String
    , kmId : String
    , version : Version
    , description : String
    , organization : OrganizationInfo
    , metamodelVersion : Int
    , forkOfPackageId : Maybe String
    , readme : String
    , versions : List Version
    , license : String
    , createdAt : Time.Posix
    }


decoder : Decoder KnowledgeModelDetail
decoder =
    D.succeed KnowledgeModelDetail
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "kmId" D.string
        |> D.required "version" Version.decoder
        |> D.required "description" D.string
        |> D.required "organization" OrganizationInfo.decoder
        |> D.required "metamodelVersion" D.int
        |> D.required "forkOfPackageId" (D.maybe D.string)
        |> D.required "readme" D.string
        |> D.required "versions" (D.list Version.decoder)
        |> D.required "license" D.string
        |> D.required "createdAt" D.datetime


getId : KnowledgeModelDetail -> String
getId km =
    km.organization.organizationId ++ ":" ++ km.kmId ++ ":" ++ Version.toString km.version


otherVersionId : KnowledgeModelDetail -> Version -> String
otherVersionId km version =
    km.organization.organizationId ++ ":" ++ km.kmId ++ ":" ++ Version.toString version
