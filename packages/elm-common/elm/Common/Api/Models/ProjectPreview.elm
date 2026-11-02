module Common.Api.Models.ProjectPreview exposing
    ( ProjectPreview
    , decoder
    , hasTemplateSet
    )

import Common.Api.Models.DocumentTemplate.DocumentTemplateFormat as DocumentTemplateFormat exposing (DocumentTemplateFormat)
import Common.Api.Models.Permission as Permission exposing (Permission)
import Common.Api.Models.Project.ProjectSharing as ProjectSharing exposing (ProjectSharing)
import Common.Api.Models.Project.ProjectVisibility as ProjectVisibility exposing (ProjectVisibility)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)


type alias ProjectPreview =
    { uuid : Uuid
    , documentTemplateUuid : Maybe Uuid
    , format : Maybe DocumentTemplateFormat
    , permissions : List Permission
    , sharing : ProjectSharing
    , visibility : ProjectVisibility
    }


decoder : Decoder ProjectPreview
decoder =
    D.succeed ProjectPreview
        |> D.required "uuid" Uuid.decoder
        |> D.required "documentTemplateUuid" (D.maybe Uuid.decoder)
        |> D.required "format" (D.maybe DocumentTemplateFormat.decoder)
        |> D.required "permissions" (D.list Permission.decoder)
        |> D.required "sharing" ProjectSharing.decoder
        |> D.required "visibility" ProjectVisibility.decoder


hasTemplateSet : ProjectPreview -> Bool
hasTemplateSet questionnairePreview =
    Maybe.isJust questionnairePreview.documentTemplateUuid && Maybe.isJust questionnairePreview.format
