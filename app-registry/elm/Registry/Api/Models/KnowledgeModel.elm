module Registry.Api.Models.KnowledgeModel exposing
    ( KnowledgeModel
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Registry.Api.Models.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import Time
import Version exposing (Version)


type alias KnowledgeModel =
    { id : String
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
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "kmId" D.string
        |> D.required "version" Version.decoder
        |> D.required "description" D.string
        |> D.required "organization" OrganizationInfo.decoder
        |> D.required "createdAt" D.datetime
