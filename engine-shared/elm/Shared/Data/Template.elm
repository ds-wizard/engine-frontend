module Shared.Data.Template exposing
    ( Template
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Shared.Data.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import Shared.Data.Template.TemplateFormat as TemplateFormat exposing (TemplateFormat)
import Shared.Data.Template.TemplateState as TemplateState exposing (TemplateState)
import Time
import Version exposing (Version)


type alias Template =
    { createdAt : Time.Posix
    , description : String
    , formats : List TemplateFormat
    , id : String
    , name : String
    , organization : Maybe OrganizationInfo
    , organizationId : String
    , recommendedPackageId : Maybe String
    , remoteLatestVersion : Maybe String
    , state : TemplateState
    , templateId : String
    , version : Version
    }


decoder : Decoder Template
decoder =
    D.succeed Template
        |> D.required "createdAt" D.datetime
        |> D.required "description" D.string
        |> D.required "formats" (D.list TemplateFormat.decoder)
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.optional "organization" (D.maybe OrganizationInfo.decoder) Nothing
        |> D.required "organizationId" D.string
        |> D.required "recommendedPackageId" (D.maybe D.string)
        |> D.required "remoteLatestVersion" (D.maybe D.string)
        |> D.required "state" TemplateState.decoder
        |> D.required "templateId" D.string
        |> D.required "version" Version.decoder
