module Wizard.Common.Components.Questionnaire.Importer exposing (ImporterResult, convertToQuestionnaireEvents)

import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import List.Extra as List
import Maybe.Extra as Maybe
import Random exposing (Seed)
import Shared.Data.KnowledgeModel as KnowledgeModel
import Shared.Data.KnowledgeModel.Question exposing (Question(..))
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent exposing (QuestionnaireEvent(..))
import Shared.Data.QuestionnaireDetail.Reply.ReplyValue exposing (ReplyValue(..), getItemUuids)
import Shared.Data.QuestionnaireDetail.Reply.ReplyValue.IntegrationReplyType as IntegrationReplyType
import Shared.Data.SummaryReport.AnsweredIndicationData as AnsweredIndicationData
import Shared.Utils exposing (flip, getUuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire.Importer.ImporterEvent as ImporterEvent exposing (ImporterEvent(..))


type alias ImporterResult =
    { questionnaireEvents : List QuestionnaireEvent
    , errors : List String
    }


initialResult : ImporterResult
initialResult =
    { questionnaireEvents = []
    , errors = []
    }


convertToQuestionnaireEvents : AppState -> QuestionnaireDetail -> E.Value -> ( Seed, ImporterResult )
convertToQuestionnaireEvents appState questionnaire data =
    case D.decodeValue (D.list ImporterEvent.decoder) data of
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
    -> QuestionnaireDetail
    -> ImporterEvent
    -> ( Seed, Dict String (List String), ImporterResult )
    -> ( Seed, Dict String (List String), ImporterResult )
foldCreateEvent appState questionnaire importerEvent result =
    createEvent appState questionnaire importerEvent result


createEvent :
    AppState
    -> QuestionnaireDetail
    -> ImporterEvent
    -> ( Seed, Dict String (List String), ImporterResult )
    -> ( Seed, Dict String (List String), ImporterResult )
createEvent appState questionnaire importerEvent ( seed, items, importerResult ) =
    let
        ( eventUuid, seed2 ) =
            getUuid seed

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
                , phasesAnsweredIndication = AnsweredIndicationData.empty
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
