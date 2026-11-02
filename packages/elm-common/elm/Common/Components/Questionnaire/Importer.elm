module Common.Components.Questionnaire.Importer exposing
    ( ImporterResult
    , convertToQuestionnaireEvents
    )

{-| What an importer plugin sends, turned into questionnaire events.

A plugin says what it found in its own words — a reply at a path, an item added
to a list — and this reads those against the knowledge model the project runs on:
a reply to a question that is not there, or of a kind that question does not
take, is an error rather than an event, and the errors are shown beside the
changes so both are read before anything is saved.

Item lists are the one thing that has to be remembered as it goes: each added
item is a new reply carrying every item before it, so the events are folded with
the items each path has gathered so far.

-}

import Common.Api.Models.KnowledgeModel as KnowledgeModel
import Common.Api.Models.KnowledgeModel.Question exposing (Question(..))
import Common.Api.Models.ProjectDetail.ProjectEvent exposing (ProjectEvent(..))
import Common.Api.Models.ProjectDetail.Reply.ReplyValue exposing (ReplyValue(..), getItemUuids)
import Common.Api.Models.ProjectDetail.Reply.ReplyValue.IntegrationReplyType as IntegrationReplyType
import Common.Api.Models.ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Common.Data.AppState exposing (AppState)
import Common.Data.ImporterEvent exposing (ImporterEvent(..))
import Dict exposing (Dict)
import Flip exposing (flip)
import Gettext exposing (gettext)
import Json.Decode as D
import List.Extra as List
import Maybe.Extra as Maybe
import Random exposing (Seed)
import String.Format as String
import Uuid.Extra as Uuid


type alias ImporterResult =
    { questionnaireEvents : List ProjectEvent
    , errors : List String
    }


initialResult : ImporterResult
initialResult =
    { questionnaireEvents = []
    , errors = []
    }


convertToQuestionnaireEvents : AppState -> ProjectQuestionnaire -> Result D.Error (List ImporterEvent) -> ( Seed, ImporterResult )
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
    -> ProjectQuestionnaire
    -> ImporterEvent
    -> ( Seed, Dict String (List String), ImporterResult )
    -> ( Seed, Dict String (List String), ImporterResult )
foldCreateEvent appState questionnaire importerEvent result =
    createEvent appState questionnaire importerEvent result


createEvent :
    AppState
    -> ProjectQuestionnaire
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
            ( seed, items, { importerResult | errors = importerResult.errors ++ [ String.format (gettext "Question not found at: %s" appState.locale) [ path ] ] } )

        replyTypeUnexpected path =
            ( seed, items, { importerResult | errors = importerResult.errors ++ [ String.format (gettext "Unexpected reply type at: %s" appState.locale) [ path ] ] } )

        answerUuidUnexpected answerUuid path =
            ( seed, items, { importerResult | errors = importerResult.errors ++ [ String.format (gettext "Unexpected answer UUID \"%s\" at: %s" appState.locale) [ answerUuid, path ] ] } )

        wrap event =
            ( seed2, items, { importerResult | questionnaireEvents = importerResult.questionnaireEvents ++ [ event ] } )
    in
    case importerEvent of
        ReplyString data ->
            case getQuestionFromPath data.path of
                Just question ->
                    case question of
                        OptionsQuestion _ optionsQuestionData ->
                            if List.member data.value optionsQuestionData.answerUuids then
                                wrap <| setReply data.path <| AnswerReply data.value

                            else
                                answerUuidUnexpected data.value data.path

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
