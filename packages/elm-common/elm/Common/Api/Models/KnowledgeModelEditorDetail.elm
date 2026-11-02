module Common.Api.Models.KnowledgeModelEditorDetail exposing
    ( KnowledgeModelEditorDetail
    , decoder
    )

import Common.Api.Models.Event as Event exposing (Event)
import Common.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Common.Api.Models.KnowledgeModelEditor.KnowledgeModelEditorState as KnowledgeModelEditorState exposing (KnowledgeModelEditorState)
import Common.Api.Models.KnowledgeModelPackage as KnowledgeModelPackage exposing (KnowledgeModelPackage)
import Common.Api.Models.KnowledgeModelPackageInfo as KnowledgeModelPackageInfo exposing (KnowledgeModelPackageInfo)
import Common.Api.Models.ProjectDetail.Reply as Reply exposing (Reply)
import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)
import Version exposing (Version)


type alias KnowledgeModelEditorDetail =
    { uuid : Uuid
    , name : String
    , description : String
    , id : String
    , language : String
    , license : String
    , readme : String
    , version : Version
    , knowledgeModel : KnowledgeModel
    , forkOfPackage : Maybe KnowledgeModelPackage
    , previousPackage : Maybe KnowledgeModelPackageInfo
    , events : List Event
    , state : KnowledgeModelEditorState
    , replies : Dict String Reply
    , workspaceUuid : Maybe Uuid
    }


decoder : Decoder KnowledgeModelEditorDetail
decoder =
    D.succeed KnowledgeModelEditorDetail
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "description" D.string
        |> D.required "id" D.string
        |> D.required "language" D.string
        |> D.required "license" D.string
        |> D.required "readme" D.string
        |> D.required "version" Version.decoder
        |> D.required "knowledgeModel" KnowledgeModel.decoder
        |> D.required "forkOfPackage" (D.nullable KnowledgeModelPackage.decoder)
        |> D.required "previousPackage" (D.nullable KnowledgeModelPackageInfo.decoder)
        |> D.required "events" (D.list Event.decoder)
        |> D.required "state" KnowledgeModelEditorState.decoder
        |> D.required "replies" (D.dict Reply.decoder)
        |> D.optional "workspaceUuid" (D.nullable Uuid.decoder) Nothing
