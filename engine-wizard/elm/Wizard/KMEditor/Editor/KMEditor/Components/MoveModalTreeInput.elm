module Wizard.KMEditor.Editor.KMEditor.Components.MoveModalTreeInput exposing
    ( Model
    , Msg
    , initialModel
    , update
    , view
    )

import Dict exposing (Dict)
import Html exposing (Html, a, div, i, li, span, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Set exposing (Set)
import Shared.Data.KnowledgeModel.Question as Question
import Shared.Html exposing (emptyNode, faKeyClass, faSet)
import Shared.Locale exposing (lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.KMEditor.Editor.KMEditor.Models.Editors exposing (Editor(..), getEditorParentUuid)


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.KMEditor.Editor.KMEditor.Components.MoveModalTreeInput"


type Msg
    = ToggleTreeOpen String
    | ChangeUuid String
    | ExpandAll
    | CollapseAll


type alias Model =
    { uuid : String
    , treeOpenUuids : Set String
    }


type MovingEntity
    = MovingQuestion
    | MovingAnswer
    | MovingChoice
    | MovingReference
    | MovingExpert
    | Other


initialModel : String -> Model
initialModel kmUuid =
    { uuid = ""
    , treeOpenUuids = Set.fromList [ kmUuid ]
    }


type alias UpdateProps =
    { editors : Dict String Editor }


update : Msg -> Model -> UpdateProps -> Model
update msg model props =
    case msg of
        ToggleTreeOpen uuid ->
            if Set.member uuid model.treeOpenUuids then
                { model | treeOpenUuids = Set.remove uuid model.treeOpenUuids }

            else
                { model | treeOpenUuids = Set.insert uuid model.treeOpenUuids }

        ChangeUuid uuid ->
            { model | uuid = uuid }

        ExpandAll ->
            { model | treeOpenUuids = Set.fromList <| Dict.keys props.editors }

        CollapseAll ->
            { model | treeOpenUuids = Set.empty }


type alias ViewProps =
    { editors : Dict String Editor
    , kmUuid : String
    , kmName : String
    , movingUuid : String
    }


view : AppState -> Model -> ViewProps -> Html Msg
view appState model props =
    let
        treeNodeProps =
            { editors = props.editors
            , movingUuid = props.movingUuid
            , movingParentUuid = Maybe.withDefault "" <| Maybe.map getEditorParentUuid <| Dict.get props.movingUuid props.editors
            , movingEntity = getMovingEntity props.editors props.movingUuid
            }
    in
    div []
        [ div [ class "diff-tree-input-actions" ]
            [ a [ onClick ExpandAll ]
                [ faSet "kmEditor.expandAll" appState
                , lx_ "expandAll" appState
                ]
            , a [ onClick CollapseAll ]
                [ faSet "kmEditor.collapseAll" appState
                , lx_ "collapseAll" appState
                ]
            ]
        , div [ class "diff-tree diff-tree-input" ]
            [ div [ class "inner" ] [ ul [] [ treeNodeKM appState model treeNodeProps props.kmName props.kmUuid ] ]
            ]
        ]


type alias TreeNodeProps =
    { editors : Dict String Editor
    , movingUuid : String
    , movingParentUuid : String
    , movingEntity : MovingEntity
    }


treeNodeKM : AppState -> Model -> TreeNodeProps -> String -> String -> Html Msg
treeNodeKM appState model props kmName kmUuid =
    case Dict.get kmUuid props.editors of
        Just (KMEditor kmEditorData) ->
            treeNode appState
                { title = kmName
                , uuid = kmEditorData.uuid
                , children = List.map (treeNodeChapter appState model props) kmEditorData.chapters.list
                , allowed = False
                , icon = faSet "km.knowledgeModel" appState
                , isOpen = isTreeOpen model kmUuid
                , isSelected = False
                }

        _ ->
            emptyNode


treeNodeChapter : AppState -> Model -> TreeNodeProps -> String -> Html Msg
treeNodeChapter appState model props chapterUuid =
    case Dict.get chapterUuid props.editors of
        Just (ChapterEditor chapterEditorData) ->
            let
                isParent =
                    props.movingParentUuid == chapterUuid

                allowed =
                    not isParent && props.movingEntity == MovingQuestion
            in
            treeNode appState
                { title = chapterEditorData.chapter.title
                , uuid = chapterUuid
                , children = List.map (treeNodeQuestion appState model props False) chapterEditorData.questions.list
                , allowed = allowed
                , icon = faSet "km.chapter" appState
                , isOpen = isTreeOpen model chapterUuid
                , isSelected = model.uuid == chapterUuid
                }

        _ ->
            emptyNode


treeNodeQuestion : AppState -> Model -> TreeNodeProps -> Bool -> String -> Html Msg
treeNodeQuestion appState model props isChild questionUuid =
    case Dict.get questionUuid props.editors of
        Just (QuestionEditor questionEditorData) ->
            let
                isSelf =
                    props.movingUuid == questionUuid

                isParent =
                    props.movingParentUuid == questionUuid

                allowed =
                    not isSelf
                        && not isParent
                        && not isChild
                        && ((props.movingEntity == MovingReference)
                                || (props.movingEntity == MovingExpert)
                                || (Question.isOptions questionEditorData.question && props.movingEntity == MovingAnswer)
                                || (Question.isMultiChoice questionEditorData.question && props.movingEntity == MovingChoice)
                                || (Question.isList questionEditorData.question && props.movingEntity == MovingQuestion)
                           )
            in
            treeNode appState
                { title = Question.getTitle questionEditorData.question
                , uuid = questionUuid
                , children =
                    List.append
                        (List.map (treeNodeAnswer appState model props (isSelf || isChild)) questionEditorData.answers.list)
                        (List.map (treeNodeQuestion appState model props (isSelf || isChild)) questionEditorData.itemTemplateQuestions.list)
                , allowed = allowed
                , icon = faSet "km.question" appState
                , isOpen = isTreeOpen model questionUuid
                , isSelected = model.uuid == questionUuid
                }

        _ ->
            emptyNode


treeNodeAnswer : AppState -> Model -> TreeNodeProps -> Bool -> String -> Html Msg
treeNodeAnswer appState model props isChild answerUuid =
    case Dict.get answerUuid props.editors of
        Just (AnswerEditor answerEditorData) ->
            let
                isSelf =
                    props.movingUuid == answerEditorData.uuid

                isParent =
                    props.movingParentUuid == answerUuid

                allowed =
                    not isSelf && not isParent && not isChild && props.movingEntity == MovingQuestion
            in
            treeNode appState
                { title = answerEditorData.answer.label
                , uuid = answerEditorData.uuid
                , children = List.map (treeNodeQuestion appState model props (isSelf || isChild)) answerEditorData.followUps.list
                , allowed = allowed
                , icon = faSet "km.answer" appState
                , isOpen = isTreeOpen model answerUuid
                , isSelected = model.uuid == answerUuid
                }

        _ ->
            emptyNode


type alias TreeNodeConfig =
    { title : String
    , uuid : String
    , icon : Html Msg
    , children : List (Html Msg)
    , allowed : Bool
    , isOpen : Bool
    , isSelected : Bool
    }


treeNode : AppState -> TreeNodeConfig -> Html Msg
treeNode appState treeNodeConfig =
    let
        caret =
            if List.length treeNodeConfig.children > 0 then
                treeNodeCaret appState (ToggleTreeOpen treeNodeConfig.uuid) treeNodeConfig.isOpen

            else
                emptyNode

        childrenList =
            if treeNodeConfig.isOpen then
                ul [] treeNodeConfig.children

            else
                emptyNode

        link =
            if treeNodeConfig.allowed then
                a [ onClick <| ChangeUuid treeNodeConfig.uuid ]

            else
                a []
    in
    li
        [ classList
            [ ( "disabled", not treeNodeConfig.allowed )
            , ( "active", treeNodeConfig.isSelected )
            ]
        , dataCy "km-editor_move-modal_item"
        ]
        [ caret
        , link [ treeNodeConfig.icon, span [] [ text treeNodeConfig.title ] ]
        , childrenList
        ]


treeNodeCaret : AppState -> Msg -> Bool -> Html Msg
treeNodeCaret appState toggleMsg isOpen =
    a
        [ onClick toggleMsg
        , class "caret"
        , dataCy "km-editor_move-modal_item_caret"
        ]
        [ i
            [ classList
                [ ( faKeyClass "kmEditor.treeClosed" appState, not isOpen )
                , ( faKeyClass "kmEditor.treeOpened" appState, isOpen )
                ]
            ]
            []
        ]


isTreeOpen : Model -> String -> Bool
isTreeOpen model uuid =
    Set.member uuid model.treeOpenUuids


getMovingEntity : Dict String Editor -> String -> MovingEntity
getMovingEntity editors uuid =
    case Dict.get uuid editors of
        Just (QuestionEditor _) ->
            MovingQuestion

        Just (AnswerEditor _) ->
            MovingAnswer

        Just (ChoiceEditor _) ->
            MovingChoice

        Just (ReferenceEditor _) ->
            MovingReference

        Just (ExpertEditor _) ->
            MovingExpert

        _ ->
            Other
