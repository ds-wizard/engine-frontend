module Questionnaires.Common.QuestionnaireMigration exposing
    ( QuestionnaireMigration
    , addResolvedQuestion
    , decoder
    , encode
    , isQuestionResolved
    , removeResolvedQuestion
    )

import Common.Questionnaire.Models exposing (QuestionnaireDetail, questionnaireDetailDecoder)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import List.Extra as List


type alias QuestionnaireMigration =
    { oldQuestionnaire : QuestionnaireDetail
    , newQuestionnaire : QuestionnaireDetail
    , resolvedQuestionUuids : List String
    }


decoder : Decoder QuestionnaireMigration
decoder =
    D.succeed QuestionnaireMigration
        |> D.required "oldQuestionnaire" questionnaireDetailDecoder
        |> D.required "newQuestionnaire" questionnaireDetailDecoder
        |> D.required "resolvedQuestionUuids" (D.list D.string)


encode : QuestionnaireMigration -> E.Value
encode migration =
    E.object
        [ ( "resolvedQuestionUuids", E.list E.string migration.resolvedQuestionUuids ) ]


addResolvedQuestion : String -> QuestionnaireMigration -> QuestionnaireMigration
addResolvedQuestion questionUuid migration =
    { migration | resolvedQuestionUuids = List.unique <| migration.resolvedQuestionUuids ++ [ questionUuid ] }


isQuestionResolved : String -> QuestionnaireMigration -> Bool
isQuestionResolved questionUuid migration =
    List.member questionUuid migration.resolvedQuestionUuids


removeResolvedQuestion : String -> QuestionnaireMigration -> QuestionnaireMigration
removeResolvedQuestion questionUuid migration =
    { migration | resolvedQuestionUuids = List.filter ((/=) questionUuid) migration.resolvedQuestionUuids }
