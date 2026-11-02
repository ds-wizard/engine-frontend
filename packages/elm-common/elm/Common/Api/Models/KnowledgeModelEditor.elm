module Common.Api.Models.KnowledgeModelEditor exposing
    ( KnowledgeModelEditor
    , decoder
    , matchState
    )

import Common.Api.Models.KnowledgeModelEditor.KnowledgeModelEditorState as KnowledgeModelEditorState exposing (KnowledgeModelEditorState)
import Common.Api.Models.KnowledgeModelPackageSuggestion as KnowledgeModelPackageSuggestion exposing (KnowledgeModelPackageSuggestion)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Uuid exposing (Uuid)


type alias KnowledgeModelEditor =
    { uuid : Uuid
    , name : String
    , id : String
    , forkOfPackage : Maybe KnowledgeModelPackageSuggestion
    , previousPackageUuid : Maybe Uuid
    , state : KnowledgeModelEditorState
    , updatedAt : Time.Posix
    , workspaceUuid : Maybe Uuid
    }


decoder : Decoder KnowledgeModelEditor
decoder =
    D.succeed KnowledgeModelEditor
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "id" D.string
        |> D.required "forkOfPackage" (D.nullable KnowledgeModelPackageSuggestion.decoder)
        |> D.required "previousPackageUuid" (D.nullable Uuid.decoder)
        |> D.required "state" KnowledgeModelEditorState.decoder
        |> D.required "updatedAt" D.datetime
        |> D.optional "workspaceUuid" (D.nullable Uuid.decoder) Nothing


matchState : List KnowledgeModelEditorState -> KnowledgeModelEditor -> Bool
matchState states kmEditor =
    List.member kmEditor.state states
