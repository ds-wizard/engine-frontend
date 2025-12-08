module Wizard.Api.Models.WebSockets.ProjectMessage.SetProjectData exposing
    ( SetProjectData
    , decoder
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)
import Wizard.Api.Models.DocumentTemplate.DocumentTemplateFormat as DocumentTemplateFormat exposing (DocumentTemplateFormat)
import Wizard.Api.Models.DocumentTemplateSuggestion as DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Wizard.Api.Models.Permission as Permission exposing (Permission)
import Wizard.Api.Models.Project.ProjectSharing as ProjectSharing exposing (ProjectSharing)
import Wizard.Api.Models.Project.ProjectVisibility as ProjectVisibility exposing (ProjectVisibility)


type alias SetProjectData =
    { name : String
    , description : Maybe String
    , projectTags : List String
    , isTemplate : Bool
    , visibility : ProjectVisibility
    , sharing : ProjectSharing
    , documentTemplateId : Maybe String
    , documentTemplate : Maybe DocumentTemplateSuggestion
    , formatUuid : Maybe Uuid
    , format : Maybe DocumentTemplateFormat
    , permissions : List Permission
    , labels : Dict String (List String)
    , unresolvedCommentCounts : Dict String (Dict String Int)
    , resolvedCommentCounts : Dict String (Dict String Int)
    }


decoder : Decoder SetProjectData
decoder =
    D.succeed SetProjectData
        |> D.required "name" D.string
        |> D.required "description" (D.maybe D.string)
        |> D.required "projectTags" (D.list D.string)
        |> D.required "isTemplate" D.bool
        |> D.required "visibility" ProjectVisibility.decoder
        |> D.required "sharing" ProjectSharing.decoder
        |> D.required "documentTemplateId" (D.maybe D.string)
        |> D.required "documentTemplate" (D.maybe DocumentTemplateSuggestion.decoder)
        |> D.required "formatUuid" (D.maybe Uuid.decoder)
        |> D.required "format" (D.maybe DocumentTemplateFormat.decoder)
        |> D.required "permissions" (D.list Permission.decoder)
        |> D.required "labels" (D.dict (D.list D.string))
        |> D.required "unresolvedCommentCounts" (D.dict (D.dict D.int))
        |> D.required "resolvedCommentCounts" (D.dict (D.dict D.int))
