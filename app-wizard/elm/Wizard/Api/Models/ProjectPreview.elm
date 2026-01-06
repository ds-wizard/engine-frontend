module Wizard.Api.Models.ProjectPreview exposing
    ( ProjectPreview
    , decoder
    , hasTemplateSet
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)
import Wizard.Api.Models.DocumentTemplate.DocumentTemplateFormat as DocumentTemplateFormat exposing (DocumentTemplateFormat)
import Wizard.Api.Models.Permission as Permission exposing (Permission)
import Wizard.Api.Models.Project.ProjectSharing as ProjectSharing exposing (ProjectSharing)
import Wizard.Api.Models.Project.ProjectVisibility as ProjectVisibility exposing (ProjectVisibility)


type alias ProjectPreview =
    { uuid : Uuid
    , documentTemplateId : Maybe String
    , format : Maybe DocumentTemplateFormat
    , permissions : List Permission
    , sharing : ProjectSharing
    , visibility : ProjectVisibility
    , migrationUuid : Maybe Uuid
    }


decoder : Decoder ProjectPreview
decoder =
    D.succeed ProjectPreview
        |> D.required "uuid" Uuid.decoder
        |> D.required "documentTemplateId" (D.maybe D.string)
        |> D.required "format" (D.maybe DocumentTemplateFormat.decoder)
        |> D.required "permissions" (D.list Permission.decoder)
        |> D.required "sharing" ProjectSharing.decoder
        |> D.required "visibility" ProjectVisibility.decoder
        |> D.required "migrationUuid" (D.maybe Uuid.decoder)


hasTemplateSet : ProjectPreview -> Bool
hasTemplateSet questionnairePreview =
    Maybe.isJust questionnairePreview.documentTemplateId && Maybe.isJust questionnairePreview.format
