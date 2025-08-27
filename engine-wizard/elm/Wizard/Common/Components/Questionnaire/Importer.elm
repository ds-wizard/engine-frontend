module Wizard.Common.Components.Questionnaire.Importer exposing
    ( ImporterResult
    , convertToQuestionnaireEvents
    )

import Dict exposing (Dict)
import Flip exposing (flip)
import Json.Decode as D
import List.Extra as List
import Maybe.Extra as Maybe
import Random exposing (Seed)
import Uuid.Extra as Uuid
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel
import Wizard.Api.Models.KnowledgeModel.Question exposing (Question(..))
import Wizard.Api.Models.QuestionnaireDetail.QuestionnaireEvent exposing (QuestionnaireEvent(..))
import Wizard.Api.Models.QuestionnaireDetail.Reply.ReplyValue exposing (ReplyValue(..), getItemUuids)
import Wizard.Api.Models.QuestionnaireDetail.Reply.ReplyValue.IntegrationReplyType as IntegrationReplyType
import Wizard.Api.Models.QuestionnaireQuestionnaire exposing (QuestionnaireQuestionnaire)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire.Importer.ImporterEvent exposing (ImporterEvent(..))


type alias ImporterResult =
    { questionnaireEvents : List QuestionnaireEvent
    , errors : List String
    }


initialResult : ImporterResult
initialResult =
    { questionnaireEvents = []
    , errors = []
    }


convertToQuestionnaireEvents : AppState -> QuestionnaireQuestionnaire -> Result D.Error (List ImporterEvent) -> ( Seed, ImporterResult )
convertToQuestionnaireEvents appState questionnaire importerEventsResult =
    case importerEventsResult of
        Ok importerEvents ->
            let
                ( newSeed, _, importerResult ) =
                    List.foldl (foldCreateEvent appState questionnaire) ( appState.seed, Dict.empty, initialResult ) importerEvents
            in
            ( newSeed, importerResult )

        Err error ->
            ( appState.seed, { initialResult | errors = [ D.errorToString error ] } )


foldCreateEvent :
    AppState
    -> QuestionnaireQuestionnaire
    -> ImporterEvent
    -> ( Seed, Dict String (List String), ImporterResult )
    -> ( Seed, Dict String (List String), ImporterResult )
foldCreateEvent appState questionnaire importerEvent result =
    createEvent appState questionnaire importerEvent result


createEvent :
    AppState
    -> QuestionnaireQuestionnaire
    -> ImporterEvent
    -> ( Seed, Dict String (List String), ImporterResult )
    -> ( Seed, Dict String (List String), ImporterResult )
createEvent appState questionnaire importerEvent ( seed, items, importerResult ) =
    let
        ( eventUuid, seed2 ) =
            Uuid.step seed

        getQuestionFromPath path =
            String.split "." path
                |> List.last
                |> Maybe.andThen (flip KnowledgeModel.getQuestion questionnaire.knowledgeModel)

        setReply path value =
            SetReply <|
                { uuid = eventUuid
                , path = path
                , value = value
                , createdAt = appState.currentTime
                , createdBy = Nothing
                }

        questionNotFound path =
            ( seed, items, { importerResult | errors = importerResult.errors ++ [ "Question not found at: " ++ path ] } )

        replyTypeUnexpected path =
            ( seed, items, { importerResult | errors = importerResult.errors ++ [ "Unexpected reply type at: " ++ path ] } )

        wrap event =
            ( seed2, items, { importerResult | questionnaireEvents = importerResult.questionnaireEvents ++ [ event ] } )
    in
    case importerEvent of
        ReplyString data ->
            case getQuestionFromPath data.path of
                Just question ->
                    case question of
                        OptionsQuestion _ _ ->
                            wrap <| setReply data.path <| AnswerReply data.value

                        IntegrationQuestion _ _ ->
                            wrap <| setReply data.path <| IntegrationReply <| IntegrationReplyType.PlainType data.value

                        ValueQuestion _ _ ->
                            wrap <| setReply data.path <| StringReply data.value

                        _ ->
                            replyTypeUnexpected data.path

                Nothing ->
                    questionNotFound data.path

        ReplyList data ->
            case getQuestionFromPath data.path of
                Just question ->
                    case question of
                        MultiChoiceQuestion _ _ ->
                            wrap <| setReply data.path <| MultiChoiceReply data.value

                        _ ->
                            replyTypeUnexpected data.path

                Nothing ->
                    questionNotFound data.path

        ReplyIntegration data ->
            case getQuestionFromPath data.path of
                Just question ->
                    case question of
                        IntegrationQuestion _ _ ->
                            wrap <| setReply data.path <| IntegrationReply <| IntegrationReplyType.IntegrationType data.value data.raw

                        _ ->
                            replyTypeUnexpected data.path

                Nothing ->
                    questionNotFound data.path

        ReplyIntegrationLegacy data ->
            case getQuestionFromPath data.path of
                Just question ->
                    case question of
                        IntegrationQuestion _ _ ->
                            wrap <| setReply data.path <| IntegrationReply <| IntegrationReplyType.IntegrationLegacyType data.id data.value

                        _ ->
                            replyTypeUnexpected data.path

                Nothing ->
                    questionNotFound data.path

        ReplyItemSelect data ->
            case getQuestionFromPath data.path of
                Just question ->
                    case question of
                        ItemSelectQuestion _ _ ->
                            wrap <| setReply data.path <| ItemSelectReply data.value

                        _ ->
                            replyTypeUnexpected data.path

                Nothing ->
                    questionNotFound data.path

        AddItem data ->
            case getQuestionFromPath data.path of
                Just question ->
                    case question of
                        ListQuestion _ _ ->
                            let
                                kmItems =
                                    Dict.get data.path questionnaire.replies
                                        |> Maybe.unwrap [] (getItemUuids << .value)

                                existingItems =
                                    Dict.get data.path items
                                        |> Maybe.withDefault kmItems

                                newExistingItems =
                                    existingItems ++ [ data.uuid ]

                                newItems =
                                    Dict.insert data.path newExistingItems items
                            in
                            ( seed2, newItems, { importerResult | questionnaireEvents = importerResult.questionnaireEvents ++ [ setReply data.path <| ItemListReply newExistingItems ] } )

                        _ ->
                            replyTypeUnexpected data.path

                Nothing ->
                    questionNotFound data.path
