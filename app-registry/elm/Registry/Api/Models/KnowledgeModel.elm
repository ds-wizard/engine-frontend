module Registry.Api.Models.KnowledgeModel exposing
    ( KnowledgeModel
    , decoder
    , getId
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Registry.Api.Models.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import Time
import Uuid exposing (Uuid)
import Version exposing (Version)


type alias KnowledgeModel =
    { uuid : Uuid
    , name : String
    , kmId : String
    , version : Version
    , description : String
    , organization : OrganizationInfo
    , createdAt : Time.Posix
    }


decoder : Decoder KnowledgeModel
decoder =
    D.succeed KnowledgeModel
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "kmId" D.string
        |> D.required "version" Version.decoder
        |> D.required "description" D.string
        |> D.required "organization" OrganizationInfo.decoder
        |> D.required "createdAt" D.datetime


getId : KnowledgeModel -> String
getId km =
    km.organization.organizationId ++ ":" ++ km.kmId ++ ":" ++ Version.toString km.version
