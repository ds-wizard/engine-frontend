module Wizard.Api.Models.ProjectMigration exposing
    ( ProjectMigration
    , addResolvedQuestion
    , addResolvedQuestions
    , decoder
    , encode
    , isQuestionResolved
    , removeResolvedQuestion
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import List.Extra as List
import Wizard.Api.Models.ProjectDetailWrapper as ProjectDetailWrapper
import Wizard.Api.Models.ProjectQuestionnaire as QuestionnaireDetail exposing (ProjectQuestionnaire)


type alias ProjectMigration =
    { oldProject : ProjectQuestionnaire
    , newProject : ProjectQuestionnaire
    , resolvedQuestionUuids : List String
    }


decoder : Decoder ProjectMigration
decoder =
    D.succeed ProjectMigration
        |> D.required "oldProject" (D.map .data (ProjectDetailWrapper.decoder QuestionnaireDetail.decoder))
        |> D.required "newProject" (D.map .data (ProjectDetailWrapper.decoder QuestionnaireDetail.decoder))
        |> D.required "resolvedQuestionUuids" (D.list D.string)


encode : ProjectMigration -> E.Value
encode migration =
    E.object
        [ ( "resolvedQuestionUuids", E.list E.string migration.resolvedQuestionUuids ) ]


addResolvedQuestion : String -> ProjectMigration -> ProjectMigration
addResolvedQuestion questionUuid migration =
    { migration | resolvedQuestionUuids = List.unique <| migration.resolvedQuestionUuids ++ [ questionUuid ] }


addResolvedQuestions : List String -> ProjectMigration -> ProjectMigration
addResolvedQuestions questionUuids migration =
    { migration | resolvedQuestionUuids = List.unique <| migration.resolvedQuestionUuids ++ questionUuids }


isQuestionResolved : String -> ProjectMigration -> Bool
isQuestionResolved questionUuid migration =
    List.member questionUuid migration.resolvedQuestionUuids


removeResolvedQuestion : String -> ProjectMigration -> ProjectMigration
removeResolvedQuestion questionUuid migration =
    { migration | resolvedQuestionUuids = List.filter ((/=) questionUuid) migration.resolvedQuestionUuids }
