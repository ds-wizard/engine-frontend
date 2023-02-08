module Registry.Common.Entities.DocumentTemplateDetail exposing
    ( DocumentTemplateDetail
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Registry.Common.Entities.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import Version exposing (Version)


type alias DocumentTemplateDetail =
    { id : String
    , name : String
    , templateId : String
    , version : Version
    , description : String
    , organization : OrganizationInfo
    , metamodelVersion : Int
    , readme : String
    , versions : List Version
    , license : String
    }


decoder : Decoder DocumentTemplateDetail
decoder =
    D.succeed DocumentTemplateDetail
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "templateId" D.string
        |> D.required "version" Version.decoder
        |> D.required "description" D.string
        |> D.required "organization" OrganizationInfo.decoder
        |> D.required "metamodelVersion" D.int
        |> D.required "readme" D.string
        |> D.required "versions" (D.list Version.decoder)
        |> D.required "license" D.string
