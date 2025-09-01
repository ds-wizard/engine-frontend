module Wizard.Api.Models.QuestionnaireMigration exposing
    ( QuestionnaireMigration
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
import Wizard.Api.Models.QuestionnaireDetailWrapper as QuestionnaireDetailWrapper
import Wizard.Api.Models.QuestionnaireQuestionnaire as QuestionnaireDetail exposing (QuestionnaireQuestionnaire)


type alias QuestionnaireMigration =
    { oldQuestionnaire : QuestionnaireQuestionnaire
    , newQuestionnaire : QuestionnaireQuestionnaire
    , resolvedQuestionUuids : List String
    }


decoder : Decoder QuestionnaireMigration
decoder =
    D.succeed QuestionnaireMigration
        |> D.required "oldQuestionnaire" (D.map .data (QuestionnaireDetailWrapper.decoder QuestionnaireDetail.decoder))
        |> D.required "newQuestionnaire" (D.map .data (QuestionnaireDetailWrapper.decoder QuestionnaireDetail.decoder))
        |> D.required "resolvedQuestionUuids" (D.list D.string)


encode : QuestionnaireMigration -> E.Value
encode migration =
    E.object
        [ ( "resolvedQuestionUuids", E.list E.string migration.resolvedQuestionUuids ) ]


addResolvedQuestion : String -> QuestionnaireMigration -> QuestionnaireMigration
addResolvedQuestion questionUuid migration =
    { migration | resolvedQuestionUuids = List.unique <| migration.resolvedQuestionUuids ++ [ questionUuid ] }


addResolvedQuestions : List String -> QuestionnaireMigration -> QuestionnaireMigration
addResolvedQuestions questionUuids migration =
    { migration | resolvedQuestionUuids = List.unique <| migration.resolvedQuestionUuids ++ questionUuids }


isQuestionResolved : String -> QuestionnaireMigration -> Bool
isQuestionResolved questionUuid migration =
    List.member questionUuid migration.resolvedQuestionUuids


removeResolvedQuestion : String -> QuestionnaireMigration -> QuestionnaireMigration
removeResolvedQuestion questionUuid migration =
    { migration | resolvedQuestionUuids = List.filter ((/=) questionUuid) migration.resolvedQuestionUuids }
