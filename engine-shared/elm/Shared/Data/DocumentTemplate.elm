module Shared.Data.DocumentTemplate exposing
    ( DocumentTemplate
    , decoder
    , isOutdated
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Shared.Data.DocumentTemplate.DocumentTemplatePhase as DocumentTemplatePhase exposing (DocumentTemplatePhase)
import Shared.Data.DocumentTemplate.DocumentTemplateState as DocumentTemplateState exposing (DocumentTemplateState)
import Shared.Data.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import Time
import Version exposing (Version)


type alias DocumentTemplate =
    { createdAt : Time.Posix
    , description : String
    , id : String
    , name : String
    , organization : Maybe OrganizationInfo
    , organizationId : String
    , phase : DocumentTemplatePhase
    , remoteLatestVersion : Maybe Version
    , state : DocumentTemplateState
    , templateId : String
    , version : Version
    , nonEditable : Bool
    }


decoder : Decoder DocumentTemplate
decoder =
    D.succeed DocumentTemplate
        |> D.required "createdAt" D.datetime
        |> D.required "description" D.string
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.optional "organization" (D.maybe OrganizationInfo.decoder) Nothing
        |> D.required "organizationId" D.string
        |> D.required "phase" DocumentTemplatePhase.decoder
        |> D.required "remoteLatestVersion" (D.maybe Version.decoder)
        |> D.required "state" DocumentTemplateState.decoder
        |> D.required "templateId" D.string
        |> D.required "version" Version.decoder
        |> D.required "nonEditable" D.bool


isOutdated : { a | remoteLatestVersion : Maybe Version, version : Version } -> Bool
isOutdated template =
    case template.remoteLatestVersion of
        Just remoteLatestVersion ->
            Version.greaterThan template.version remoteLatestVersion

        Nothing ->
            False
