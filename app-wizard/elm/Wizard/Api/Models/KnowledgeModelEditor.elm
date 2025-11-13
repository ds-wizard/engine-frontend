module Wizard.Api.Models.KnowledgeModelEditor exposing
    ( KnowledgeModelEditor
    , decoder
    , matchState
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Uuid exposing (Uuid)
import Wizard.Api.Models.KnowledgeModelEditor.KnowledgeModelEditorState as KnowledgeModelEditorState exposing (KnowledgeModelEditorState)


type alias KnowledgeModelEditor =
    { uuid : Uuid
    , name : String
    , kmId : String
    , forkOfPackageId : Maybe String
    , previousPackageId : Maybe String
    , state : KnowledgeModelEditorState
    , updatedAt : Time.Posix
    }


decoder : Decoder KnowledgeModelEditor
decoder =
    D.succeed KnowledgeModelEditor
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "kmId" D.string
        |> D.required "forkOfPackageId" (D.nullable D.string)
        |> D.required "previousPackageId" (D.nullable D.string)
        |> D.required "state" KnowledgeModelEditorState.decoder
        |> D.required "updatedAt" D.datetime


matchState : List KnowledgeModelEditorState -> KnowledgeModelEditor -> Bool
matchState states kmEditor =
    List.member kmEditor.state states
