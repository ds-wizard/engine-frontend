module Common.Api.Models.ProjectCommon exposing
    ( ProjectCommon
    , decoder
    , dummy
    , encode
    , updateWithQuestionnaireData
    , updateWithShareData
    )

import Common.Api.Models.KnowledgeModelPackage as KnowledgeModelPackage
import Common.Api.Models.KnowledgeModelPackageSuggestion as KnowledgeModelPackageSuggestion exposing (KnowledgeModelPackageSuggestion)
import Common.Api.Models.Permission as Permission exposing (Permission)
import Common.Api.Models.Project.ProjectSharing as ProjectSharing exposing (ProjectSharing)
import Common.Api.Models.Project.ProjectVisibility as ProjectVisibility exposing (ProjectVisibility)
import Common.Api.Models.WebSockets.ProjectMessage.SetProjectData exposing (SetProjectData)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Uuid exposing (Uuid)


type alias ProjectCommon =
    { uuid : Uuid
    , name : String
    , isTemplate : Bool
    , permissions : List Permission
    , sharing : ProjectSharing
    , visibility : ProjectVisibility
    , knowledgeModelPackage : KnowledgeModelPackageSuggestion
    , fileCount : Int
    , workspaceUuid : Maybe Uuid
    }


decoder : Decoder ProjectCommon
decoder =
    D.succeed ProjectCommon
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "isTemplate" D.bool
        |> D.required "permissions" (D.list Permission.decoder)
        |> D.required "sharing" ProjectSharing.decoder
        |> D.required "visibility" ProjectVisibility.decoder
        |> D.required "knowledgeModelPackage" KnowledgeModelPackageSuggestion.decoder
        |> D.required "fileCount" D.int
        |> D.optional "workspaceUuid" (D.nullable Uuid.decoder) Nothing


encode : ProjectCommon -> E.Value
encode p =
    E.object
        [ ( "uuid", Uuid.encode p.uuid )
        , ( "name", E.string p.name )
        , ( "isTemplate", E.bool p.isTemplate )
        , ( "knowledgeModelPackage", KnowledgeModelPackageSuggestion.encode p.knowledgeModelPackage )
        , ( "workspaceUuid", E.maybe Uuid.encode p.workspaceUuid )
        ]


updateWithQuestionnaireData : SetProjectData -> ProjectCommon -> ProjectCommon
updateWithQuestionnaireData data project =
    { project
        | name = data.name
        , isTemplate = data.isTemplate
        , permissions = data.permissions
        , sharing = data.sharing
        , visibility = data.visibility
    }


updateWithShareData : { a | permissions : List Permission, sharing : ProjectSharing, visibility : ProjectVisibility } -> ProjectCommon -> ProjectCommon
updateWithShareData data project =
    { project
        | permissions = data.permissions
        , sharing = data.sharing
        , visibility = data.visibility
    }


dummy : ProjectCommon
dummy =
    { uuid = Uuid.nil
    , name = ""
    , isTemplate = False
    , permissions = []
    , sharing = ProjectSharing.Restricted
    , visibility = ProjectVisibility.Private
    , knowledgeModelPackage = KnowledgeModelPackageSuggestion.fromKnowledgeModelPackage KnowledgeModelPackage.dummy
    , fileCount = 0
    , workspaceUuid = Nothing
    }
