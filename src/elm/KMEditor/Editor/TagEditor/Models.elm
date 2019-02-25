module KMEditor.Editor.TagEditor.Models exposing
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
import KMEditor.Common.Models.Entities exposing (KnowledgeModel, Question(..), createPathMap, getAllQuestions, getQuestionTagUuids, getQuestionUuid)
import KMEditor.Common.Models.Events exposing (EditQuestionEventData(..), Event(..), EventField, createEmptyEventField, createEventField)
import KMEditor.Common.Models.Path exposing (Path)
import Random exposing (Seed)
import Utils exposing (getUuid)


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
    List.foldl (\q dict -> Dict.insert (getQuestionUuid q) (getQuestionTagUuids q) dict) Dict.empty (getAllQuestions km)


generateEvents : Seed -> KnowledgeModel -> Model -> ( Seed, List Event )
generateEvents seed knowledgeModel model =
    let
        pathMap =
            createPathMap knowledgeModel
    in
    getAllQuestions knowledgeModel
        |> List.foldl
            (\q ( s, events ) ->
                let
                    path =
                        getPath pathMap (getQuestionUuid q)

                    ( newSeed, newEvents ) =
                        generateQuestionEvent model q path s
                in
                ( newSeed, events ++ newEvents )
            )
            ( seed, [] )


getPath : Dict String Path -> String -> Path
getPath pathMap uuid =
    case Dict.get uuid pathMap of
        Just path ->
            path

        Nothing ->
            []


generateQuestionEvent : Model -> Question -> Path -> Seed -> ( Seed, List Event )
generateQuestionEvent model question path seed =
    let
        questionUuid =
            getQuestionUuid question

        originalTags =
            List.sort <| getQuestionTagUuids question

        newTags =
            List.sort <| getQuestionTags model questionUuid
    in
    if originalTags /= newTags then
        let
            ( uuid, newSeed ) =
                getUuid seed

            eventData =
                case question of
                    OptionsQuestion _ ->
                        EditOptionsQuestionEvent
                            { questionUuid = questionUuid
                            , title = createEmptyEventField
                            , text = createEmptyEventField
                            , requiredLevel = createEmptyEventField
                            , tagUuids = createEventField newTags True
                            , referenceUuids = createEmptyEventField
                            , expertUuids = createEmptyEventField
                            , answerUuids = createEmptyEventField
                            }

                    ListQuestion _ ->
                        EditListQuestionEvent
                            { questionUuid = questionUuid
                            , title = createEmptyEventField
                            , text = createEmptyEventField
                            , requiredLevel = createEmptyEventField
                            , tagUuids = createEventField newTags True
                            , referenceUuids = createEmptyEventField
                            , expertUuids = createEmptyEventField
                            , itemTemplateTitle = createEmptyEventField
                            , itemTemplateQuestionUuids = createEmptyEventField
                            }

                    ValueQuestion _ ->
                        EditValueQuestionEvent
                            { questionUuid = questionUuid
                            , title = createEmptyEventField
                            , text = createEmptyEventField
                            , requiredLevel = createEmptyEventField
                            , tagUuids = createEventField newTags True
                            , referenceUuids = createEmptyEventField
                            , expertUuids = createEmptyEventField
                            , valueType = createEmptyEventField
                            }

            event =
                EditQuestionEvent
                    eventData
                    { uuid = uuid, path = path }
        in
        ( seed, [ event ] )

    else
        ( seed, [] )
