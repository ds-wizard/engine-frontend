module Wizard.Api.Models.KnowledgeModelEditorDetail exposing
    ( KnowledgeModelEditorDetail
    , decoder
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)
import Version exposing (Version)
import Wizard.Api.Models.Event as Event exposing (Event)
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.KnowledgeModelEditor.KnowledgeModelEditorState as KnowledgeModelEditorState exposing (KnowledgeModelEditorState)
import Wizard.Api.Models.KnowledgeModelPackage as KnowledgeModelPackage exposing (KnowledgeModelPackage)
import Wizard.Api.Models.QuestionnaireDetail.Reply as Reply exposing (Reply)


type alias KnowledgeModelEditorDetail =
    { uuid : Uuid
    , name : String
    , description : String
    , kmId : String
    , license : String
    , readme : String
    , version : Version
    , knowledgeModel : KnowledgeModel
    , forkOfPackageId : Maybe String
    , forkOfPackage : Maybe KnowledgeModelPackage
    , previousPackageId : Maybe String
    , events : List Event
    , state : KnowledgeModelEditorState
    , replies : Dict String Reply
    }


decoder : Decoder KnowledgeModelEditorDetail
decoder =
    D.succeed KnowledgeModelEditorDetail
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "description" D.string
        |> D.required "kmId" D.string
        |> D.required "license" D.string
        |> D.required "readme" D.string
        |> D.required "version" Version.decoder
        |> D.required "knowledgeModel" KnowledgeModel.decoder
        |> D.required "forkOfPackageId" (D.nullable D.string)
        |> D.required "forkOfPackage" (D.nullable KnowledgeModelPackage.decoder)
        |> D.required "previousPackageId" (D.nullable D.string)
        |> D.required "events" (D.list Event.decoder)
        |> D.required "state" KnowledgeModelEditorState.decoder
        |> D.required "replies" (D.dict Reply.decoder)
