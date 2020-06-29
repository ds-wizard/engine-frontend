module Registry.Common.Entities.Template exposing
    ( Template
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Registry.Common.Entities.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import Version exposing (Version)


type alias Template =
    { id : String
    , name : String
    , templateId : String
    , version : Version
    , description : String
    , organization : OrganizationInfo
    }


decoder : Decoder Template
decoder =
    D.succeed Template
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "templateId" D.string
        |> D.required "version" Version.decoder
        |> D.required "description" D.string
        |> D.required "organization" OrganizationInfo.decoder
