module Common.Api.Models.Project exposing
    ( Project
    , decoder
    )

import Common.Api.Models.KnowledgeModelPackageInfo as KnowledgeModelPackageInfo exposing (KnowledgeModelPackageInfo)
import Common.Api.Models.Permission as Permission exposing (Permission)
import Common.Api.Models.Project.DocumentTemplateProjectState as DocumentTemplateProjectState exposing (DocumentTemplateProjectState)
import Common.Api.Models.Project.KnowledgeModelProjectState as KnowledgeModelProjectState exposing (KnowledgeModelProjectState)
import Common.Api.Models.Project.ProjectSharing as ProjectSharing exposing (ProjectSharing)
import Common.Api.Models.Project.ProjectVisibility as ProjectVisibility exposing (ProjectVisibility)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Uuid exposing (Uuid)


type alias Project =
    { uuid : Uuid
    , name : String
    , description : Maybe String
    , isTemplate : Bool
    , knowledgeModelPackage : KnowledgeModelPackageInfo
    , visibility : ProjectVisibility
    , sharing : ProjectSharing
    , permissions : List Permission
    , knowledgeModelState : KnowledgeModelProjectState
    , documentTemplateState : Maybe DocumentTemplateProjectState
    , updatedAt : Time.Posix
    , workspaceUuid : Maybe Uuid
    }


decoder : Decoder Project
decoder =
    D.succeed Project
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "description" (D.maybe D.string)
        |> D.required "isTemplate" D.bool
        |> D.required "knowledgeModelPackage" KnowledgeModelPackageInfo.decoder
        |> D.required "visibility" ProjectVisibility.decoder
        |> D.required "sharing" ProjectSharing.decoder
        |> D.required "permissions" (D.list Permission.decoder)
        |> D.required "knowledgeModelState" KnowledgeModelProjectState.decoder
        |> D.required "documentTemplateState" (D.maybe DocumentTemplateProjectState.decoder)
        |> D.required "updatedAt" D.datetime
        |> D.optional "workspaceUuid" (D.nullable Uuid.decoder) Nothing
