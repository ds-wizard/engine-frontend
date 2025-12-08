module Wizard.Api.Models.ProjectCommon exposing
    ( ProjectCommon
    , decoder
    , updateWithQuestionnaireData
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)
import Wizard.Api.Models.Permission as Permission exposing (Permission)
import Wizard.Api.Models.Project.ProjectSharing as ProjectSharing exposing (ProjectSharing)
import Wizard.Api.Models.Project.ProjectVisibility as ProjectVisibility exposing (ProjectVisibility)
import Wizard.Api.Models.WebSockets.ProjectMessage.SetProjectData exposing (SetProjectData)


type alias ProjectCommon =
    { uuid : Uuid
    , name : String
    , isTemplate : Bool
    , permissions : List Permission
    , sharing : ProjectSharing
    , visibility : ProjectVisibility
    , migrationUuid : Maybe Uuid
    , knowledgeModelPackageId : String
    , fileCount : Int
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
        |> D.required "migrationUuid" (D.nullable Uuid.decoder)
        |> D.required "knowledgeModelPackageId" D.string
        |> D.required "fileCount" D.int


updateWithQuestionnaireData : SetProjectData -> ProjectCommon -> ProjectCommon
updateWithQuestionnaireData data project =
    { project
        | name = data.name
        , isTemplate = data.isTemplate
        , permissions = data.permissions
        , sharing = data.sharing
        , visibility = data.visibility
    }
