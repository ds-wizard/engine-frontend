module Wizard.KMEditor.Editor.Components.KMEditor.TreeInput exposing
    ( Model
    , MovingEntity(..)
    , Msg
    , ViewProps
    , initialModel
    , update
    , view
    )

import Gettext exposing (gettext)
import Html exposing (Html, a, div, i, li, span, text, ul)
import Html.Attributes exposing (attribute, class, classList)
import Html.Events exposing (onClick)
import Set exposing (Set)
import Shared.Html exposing (emptyNode, faKeyClass, faSet)
import Uuid
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel
import Wizard.Api.Models.KnowledgeModel.Answer exposing (Answer)
import Wizard.Api.Models.KnowledgeModel.Chapter exposing (Chapter)
import Wizard.Api.Models.KnowledgeModel.Expert exposing (Expert)
import Wizard.Api.Models.KnowledgeModel.Question as Question exposing (Question)
import Wizard.Api.Models.KnowledgeModel.Reference as Reference exposing (Reference)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.KMEditor.Editor.Common.EditorBranch as EditorBranch exposing (EditorBranch)


type alias Model =
    { selected : String
    , treeOpenUuids : Set String
    }


initialModel : Set String -> Model
initialModel openUuids =
    { selected = ""
    , treeOpenUuids = openUuids
    }


isTreeOpen : String -> Model -> Bool
isTreeOpen uuid model =
    Set.member uuid model.treeOpenUuids


isSelected : String -> Model -> Bool
isSelected uuid model =
    uuid == model.selected


type Msg
    = ToggleTreeOpen String
    | Select String
    | ExpandAll
    | CollapseAll


update : Msg -> EditorBranch -> Model -> Model
update msg editorBranch model =
    case msg of
        ToggleTreeOpen uuid ->
            if Set.member uuid model.treeOpenUuids then
                { model | treeOpenUuids = Set.remove uuid model.treeOpenUuids }

            else
                { model | treeOpenUuids = Set.insert uuid model.treeOpenUuids }

        Select uuid ->
            { model | selected = uuid }

        ExpandAll ->
            { model | treeOpenUuids = Set.fromList (EditorBranch.getAllUuids editorBranch) }

        CollapseAll ->
            { model | treeOpenUuids = Set.empty }


type MovingEntity
    = MovingQuestion
    | MovingAnswer
    | MovingChoice
    | MovingReference
    | MovingExpert


type alias ViewProps =
    { editorBranch : EditorBranch
    , movingUuid : String
    , movingParentUuid : String
    , movingEntity : MovingEntity
    }


view : AppState -> ViewProps -> Model -> Html Msg
view appState props model =
    div []
        [ div [ class "diff-tree-input-actions" ]
            [ a [ onClick ExpandAll ]
                [ faSet "kmEditor.expandAll" appState
                , text (gettext "Expand all" appState.locale)
                ]
            , a
                [ onClick CollapseAll
                , dataCy "km-editor_move-modal_collapse-all"
                ]
                [ faSet "kmEditor.collapseAll" appState
                , text (gettext "Collapse all" appState.locale)
                ]
            ]
        , div [ class "diff-tree diff-tree-input" ]
            [ ul [] [ treeNodeKM appState props model ] ]
        ]


treeNodeKM : AppState -> ViewProps -> Model -> Html Msg
treeNodeKM appState props model =
    let
        knowledgeModel =
            props.editorBranch.branch.knowledgeModel

        uuid =
            Uuid.toString knowledgeModel.uuid

        chapters =
            KnowledgeModel.getChapters knowledgeModel
                |> EditorBranch.filterDeletedWith .uuid props.editorBranch
                |> List.map (treeNodeChapter appState props model)

        config =
            { uuid = uuid
            , icon = faSet "km.knowledgeModel" appState
            , label = props.editorBranch.branch.name
            , children = chapters
            , untitledLabel = ""
            , allowed = False
            , open = isTreeOpen uuid model
            , selected = isSelected uuid model
            , current = False
            }
    in
    treeNode appState config


treeNodeChapter : AppState -> ViewProps -> Model -> Chapter -> Html Msg
treeNodeChapter appState props model chapter =
    let
        isParent =
            props.movingParentUuid == chapter.uuid

        allowed =
            not isParent && props.movingEntity == MovingQuestion

        questions =
            KnowledgeModel.getChapterQuestions chapter.uuid props.editorBranch.branch.knowledgeModel
                |> EditorBranch.filterDeletedWith Question.getUuid props.editorBranch
                |> List.map (treeNodeQuestion appState props model False)

        config =
            { uuid = chapter.uuid
            , icon = faSet "km.chapter" appState
            , label = chapter.title
            , children = questions
            , untitledLabel = gettext "Untitled chapter" appState.locale
            , allowed = allowed
            , open = isTreeOpen chapter.uuid model
            , selected = isSelected chapter.uuid model
            , current = chapter.uuid == props.movingUuid
            }
    in
    treeNode appState config


treeNodeQuestion : AppState -> ViewProps -> Model -> Bool -> Question -> Html Msg
treeNodeQuestion appState props model isChild question =
    let
        uuid =
            Question.getUuid question

        isSelf =
            props.movingUuid == uuid

        isParent =
            props.movingParentUuid == uuid

        allowed =
            not isSelf
                && not isParent
                && not isChild
                && ((props.movingEntity == MovingReference)
                        || (props.movingEntity == MovingExpert)
                        || (Question.isOptions question && props.movingEntity == MovingAnswer)
                        || (Question.isMultiChoice question && props.movingEntity == MovingChoice)
                        || (Question.isList question && props.movingEntity == MovingQuestion)
                   )

        answers =
            KnowledgeModel.getQuestionAnswers uuid props.editorBranch.branch.knowledgeModel
                |> EditorBranch.filterDeletedWith .uuid props.editorBranch
                |> List.map (treeNodeAnswer appState props model (isSelf || isChild))

        itemTemplateQuestions =
            KnowledgeModel.getQuestionItemTemplateQuestions uuid props.editorBranch.branch.knowledgeModel
                |> EditorBranch.filterDeletedWith Question.getUuid props.editorBranch
                |> List.map (treeNodeQuestion appState props model (isSelf || isChild))

        experts =
            KnowledgeModel.getQuestionExperts uuid props.editorBranch.branch.knowledgeModel
                |> List.filter (\e -> e.uuid == props.movingUuid)
                |> List.map (treeNodeExpert appState props)

        references =
            KnowledgeModel.getQuestionReferences uuid props.editorBranch.branch.knowledgeModel
                |> List.filter (\r -> Reference.getUuid r == props.movingUuid)
                |> List.map (treeNodeReference appState props)

        config =
            { uuid = uuid
            , icon = faSet "km.question" appState
            , label = Question.getTitle question
            , children = answers ++ itemTemplateQuestions ++ experts ++ references
            , untitledLabel = gettext "Untitled question" appState.locale
            , allowed = allowed
            , open = isTreeOpen uuid model
            , selected = isSelected uuid model
            , current = isSelf
            }
    in
    treeNode appState config


treeNodeAnswer : AppState -> ViewProps -> Model -> Bool -> Answer -> Html Msg
treeNodeAnswer appState props model isChild answer =
    let
        isSelf =
            props.movingUuid == answer.uuid

        isParent =
            props.movingParentUuid == answer.uuid

        allowed =
            not isSelf && not isParent && not isChild && props.movingEntity == MovingQuestion

        followupQuestions =
            KnowledgeModel.getAnswerFollowupQuestions answer.uuid props.editorBranch.branch.knowledgeModel
                |> EditorBranch.filterDeletedWith Question.getUuid props.editorBranch
                |> List.map (treeNodeQuestion appState props model (isSelf || isChild))

        config =
            { uuid = answer.uuid
            , icon = faSet "km.answer" appState
            , label = answer.label
            , children = followupQuestions
            , untitledLabel = gettext "Untitled answer" appState.locale
            , allowed = allowed
            , open = isTreeOpen answer.uuid model
            , selected = isSelected answer.uuid model
            , current = isSelf
            }
    in
    treeNode appState config


treeNodeExpert : AppState -> ViewProps -> Expert -> Html Msg
treeNodeExpert appState props expert =
    let
        isSelf =
            props.movingUuid == expert.uuid

        config =
            { uuid = expert.uuid
            , icon = faSet "km.expert" appState
            , label = expert.name
            , children = []
            , untitledLabel = gettext "Untitled expert" appState.locale
            , allowed = False
            , open = False
            , selected = False
            , current = isSelf
            }
    in
    treeNode appState config


treeNodeReference : AppState -> ViewProps -> Reference -> Html Msg
treeNodeReference appState props reference =
    let
        isSelf =
            props.movingUuid == Reference.getUuid reference

        config =
            { uuid = Reference.getUuid reference
            , icon = faSet "km.reference" appState
            , label = Reference.getVisibleName (KnowledgeModel.getAllResourcePages props.editorBranch.branch.knowledgeModel) reference
            , children = []
            , untitledLabel = gettext "Untitled reference" appState.locale
            , allowed = False
            , open = False
            , selected = False
            , current = isSelf
            }
    in
    treeNode appState config


type alias TreeNodeConfig msg =
    { uuid : String
    , icon : Html msg
    , label : String
    , children : List (Html msg)
    , untitledLabel : String
    , allowed : Bool
    , open : Bool
    , selected : Bool
    , current : Bool
    }


treeNode : AppState -> TreeNodeConfig Msg -> Html Msg
treeNode appState config =
    let
        caret =
            if List.isEmpty config.children then
                emptyNode

            else
                treeNodeCaret appState (ToggleTreeOpen config.uuid) config.open

        children =
            if config.open then
                ul [] config.children

            else
                emptyNode

        link =
            if config.allowed then
                a [ onClick <| Select config.uuid ]

            else
                a []

        ( untitled, visibleLabel ) =
            if String.isEmpty config.label then
                ( True, config.untitledLabel )

            else
                ( False, config.label )

        currentDataAttribute =
            if config.current then
                [ attribute "data-km-editor_move-modal_item_current" "" ]

            else
                []
    in
    li
        ([ classList
            [ ( "disabled", not config.allowed )
            , ( "active", config.selected )
            , ( "current", config.current )
            ]
         , dataCy "km-editor_move-modal_item"
         ]
            ++ currentDataAttribute
        )
        [ caret
        , link
            [ config.icon
            , span [ classList [ ( "untitled", untitled ) ] ] [ text visibleLabel ]
            ]
        , children
        ]


treeNodeCaret : AppState -> Msg -> Bool -> Html Msg
treeNodeCaret appState msg isOpen =
    a [ class "caret", onClick msg, dataCy "km-editor_move-modal_item_caret" ]
        [ i
            [ classList
                [ ( faKeyClass "kmEditor.treeClosed" appState, not isOpen )
                , ( faKeyClass "kmEditor.treeOpened" appState, isOpen )
                ]
            ]
            []
        ]
