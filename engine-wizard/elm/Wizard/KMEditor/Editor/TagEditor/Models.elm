module Wizard.KMEditor.Editor.TagEditor.Models exposing
    ( Model
    , addQuestionTag
    , containsChanges
    , generateEvents
    , hasQuestionTag
    , initialModel
    , removeQuestionTag
    )

import ActionResult exposing (ActionResult(..))
import Dict exposing (Dict)
import Random exposing (Seed)
import Shared.Data.Event exposing (Event(..))
import Shared.Data.Event.EditQuestionEventData exposing (EditQuestionEventData(..))
import Shared.Data.Event.EventField as EventField
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.KnowledgeModel.Question as Question exposing (Question(..))
import Shared.Utils exposing (getUuid)


type alias Model =
    { knowledgeModel : KnowledgeModel
    , highlightedTagUuid : Maybe String
    , questionTagsDict : Dict String (List String)
    , dirty : Bool
    , submitting : ActionResult String
    }


initialModel : KnowledgeModel -> Model
initialModel km =
    { knowledgeModel = km
    , highlightedTagUuid = Nothing
    , questionTagsDict = initQuestionTagsDict km
    , dirty = False
    , submitting = Unset
    }


hasQuestionTag : Model -> String -> String -> Bool
hasQuestionTag model questionUuid tagUuid =
    case Dict.get questionUuid model.questionTagsDict of
        Just tags ->
            List.member tagUuid tags

        Nothing ->
            False


addQuestionTag : Model -> String -> String -> Model
addQuestionTag model questionUuid tagUuid =
    let
        newTags =
            tagUuid :: getQuestionTags model questionUuid
    in
    { model
        | questionTagsDict = Dict.insert questionUuid newTags model.questionTagsDict
        , dirty = True
    }


removeQuestionTag : Model -> String -> String -> Model
removeQuestionTag model questionUUid tagUuid =
    let
        newTags =
            List.filter (\t -> t /= tagUuid) <| getQuestionTags model questionUUid
    in
    { model
        | questionTagsDict = Dict.insert questionUUid newTags model.questionTagsDict
        , dirty = True
    }


getQuestionTags : Model -> String -> List String
getQuestionTags model questionUuid =
    case Dict.get questionUuid model.questionTagsDict of
        Just tags ->
            tags

        Nothing ->
            []


containsChanges : Model -> Bool
containsChanges =
    .dirty


initQuestionTagsDict : KnowledgeModel -> Dict String (List String)
initQuestionTagsDict km =
    List.foldl (\q dict -> Dict.insert (Question.getUuid q) (Question.getTagUuids q) dict) Dict.empty (KnowledgeModel.getAllQuestions km)


generateEvents : Seed -> KnowledgeModel -> Model -> ( Seed, List Event )
generateEvents seed knowledgeModel model =
    let
        parentMap =
            KnowledgeModel.createParentMap knowledgeModel
    in
    KnowledgeModel.getAllQuestions knowledgeModel
        |> List.foldl
            (\q ( s, events ) ->
                let
                    parentUuid =
                        KnowledgeModel.getParent parentMap (Question.getUuid q)

                    ( newSeed, newEvents ) =
                        generateQuestionEvent model q parentUuid s
                in
                ( newSeed, events ++ newEvents )
            )
            ( seed, [] )


generateQuestionEvent : Model -> Question -> String -> Seed -> ( Seed, List Event )
generateQuestionEvent model question parentUuid seed =
    let
        questionUuid =
            Question.getUuid question

        originalTags =
            List.sort <| Question.getTagUuids question

        newTags =
            List.sort <| getQuestionTags model questionUuid
    in
    if originalTags /= newTags then
        let
            ( uuid, newSeed ) =
                getUuid seed

            commonData =
                { uuid = uuid
                , parentUuid = parentUuid
                , entityUuid = questionUuid
                }

            eventData =
                case question of
                    OptionsQuestion _ _ ->
                        EditQuestionOptionsEvent
                            { title = EventField.empty
                            , text = EventField.empty
                            , requiredLevel = EventField.empty
                            , tagUuids = EventField.create newTags True
                            , referenceUuids = EventField.empty
                            , expertUuids = EventField.empty
                            , answerUuids = EventField.empty
                            }

                    ListQuestion _ _ ->
                        EditQuestionListEvent
                            { title = EventField.empty
                            , text = EventField.empty
                            , requiredLevel = EventField.empty
                            , tagUuids = EventField.create newTags True
                            , referenceUuids = EventField.empty
                            , expertUuids = EventField.empty
                            , itemTemplateQuestionUuids = EventField.empty
                            }

                    ValueQuestion _ _ ->
                        EditQuestionValueEvent
                            { title = EventField.empty
                            , text = EventField.empty
                            , requiredLevel = EventField.empty
                            , tagUuids = EventField.create newTags True
                            , referenceUuids = EventField.empty
                            , expertUuids = EventField.empty
                            , valueType = EventField.empty
                            }

                    IntegrationQuestion _ _ ->
                        EditQuestionIntegrationEvent
                            { title = EventField.empty
                            , text = EventField.empty
                            , requiredLevel = EventField.empty
                            , tagUuids = EventField.create newTags True
                            , referenceUuids = EventField.empty
                            , expertUuids = EventField.empty
                            , integrationUuid = EventField.empty
                            , props = EventField.empty
                            }

            event =
                EditQuestionEvent eventData commonData
        in
        ( newSeed, [ event ] )

    else
        ( seed, [] )
