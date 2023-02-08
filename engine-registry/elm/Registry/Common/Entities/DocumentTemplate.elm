module Registry.Common.Entities.DocumentTemplate exposing
    ( DocumentTemplate
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Registry.Common.Entities.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import Version exposing (Version)


type alias DocumentTemplate =
    { id : String
    , name : String
    , templateId : String
    , version : Version
    , description : String
    , organization : OrganizationInfo
    }


decoder : Decoder DocumentTemplate
decoder =
    D.succeed DocumentTemplate
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "templateId" D.string
        |> D.required "version" Version.decoder
        |> D.required "description" D.string
        |> D.required "organization" OrganizationInfo.decoder
