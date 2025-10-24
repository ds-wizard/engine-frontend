module Wizard.Pages.KMEditor.Editor.Components.KMEditor.TreeInput exposing
    ( Model
    , MovingEntity(..)
    , Msg
    , ViewProps
    , initialModel
    , update
    , view
    )

import Common.Components.FontAwesome exposing (faKmAnswer, faKmChapter, faKmEditorCollapseAll, faKmEditorExpandAll, faKmEditorTreeClosed, faKmEditorTreeOpened, faKmExpert, faKmKnowledgeModel, faKmQuestion, faKmReference)
import Gettext exposing (gettext)
import Html exposing (Html, a, div, li, span, text, ul)
import Html.Attributes exposing (attribute, class, classList)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Set exposing (Set)
import Uuid
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel
import Wizard.Api.Models.KnowledgeModel.Answer exposing (Answer)
import Wizard.Api.Models.KnowledgeModel.Chapter exposing (Chapter)
import Wizard.Api.Models.KnowledgeModel.Expert exposing (Expert)
import Wizard.Api.Models.KnowledgeModel.Question as Question exposing (Question)
import Wizard.Api.Models.KnowledgeModel.Reference as Reference exposing (Reference)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KMEditor.Editor.Common.EditorContext as EditorContext exposing (EditorContext)


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


update : Msg -> EditorContext -> Model -> Model
update msg editorContext model =
    case msg of
        ToggleTreeOpen uuid ->
            if Set.member uuid model.treeOpenUuids then
                { model | treeOpenUuids = Set.remove uuid model.treeOpenUuids }

            else
                { model | treeOpenUuids = Set.insert uuid model.treeOpenUuids }

        Select uuid ->
            { model | selected = uuid }

        ExpandAll ->
            { model | treeOpenUuids = Set.fromList (EditorContext.getAllUuids editorContext) }

        CollapseAll ->
            { model | treeOpenUuids = Set.empty }


type MovingEntity
    = MovingQuestion
    | MovingAnswer
    | MovingChoice
    | MovingReference
    | MovingExpert


type alias ViewProps =
    { editorContext : EditorContext
    , movingUuid : String
    , movingParentUuid : String
    , movingEntity : MovingEntity
    }


view : AppState -> ViewProps -> Model -> Html Msg
view appState props model =
    div []
        [ div [ class "diff-tree-input-actions" ]
            [ a [ onClick ExpandAll ]
                [ faKmEditorExpandAll
                , text (gettext "Expand all" appState.locale)
                ]
            , a
                [ onClick CollapseAll
                , dataCy "km-editor_move-modal_collapse-all"
                ]
                [ faKmEditorCollapseAll
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
            props.editorContext.kmEditor.knowledgeModel

        uuid =
            Uuid.toString knowledgeModel.uuid

        chapters =
            KnowledgeModel.getChapters knowledgeModel
                |> EditorContext.filterDeletedWith .uuid props.editorContext
                |> List.map (treeNodeChapter appState props model)

        config =
            { uuid = uuid
            , icon = faKmKnowledgeModel
            , label = props.editorContext.kmEditor.name
            , children = chapters
            , untitledLabel = ""
            , allowed = False
            , open = isTreeOpen uuid model
            , selected = isSelected uuid model
            , current = False
            }
    in
    treeNode config


treeNodeChapter : AppState -> ViewProps -> Model -> Chapter -> Html Msg
treeNodeChapter appState props model chapter =
    let
        isParent =
            props.movingParentUuid == chapter.uuid

        allowed =
            not isParent && props.movingEntity == MovingQuestion

        questions =
            KnowledgeModel.getChapterQuestions chapter.uuid props.editorContext.kmEditor.knowledgeModel
                |> EditorContext.filterDeletedWith Question.getUuid props.editorContext
                |> List.map (treeNodeQuestion appState props model False)

        config =
            { uuid = chapter.uuid
            , icon = faKmChapter
            , label = chapter.title
            , children = questions
            , untitledLabel = gettext "Untitled chapter" appState.locale
            , allowed = allowed
            , open = isTreeOpen chapter.uuid model
            , selected = isSelected chapter.uuid model
            , current = chapter.uuid == props.movingUuid
            }
    in
    treeNode config


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
            KnowledgeModel.getQuestionAnswers uuid props.editorContext.kmEditor.knowledgeModel
                |> EditorContext.filterDeletedWith .uuid props.editorContext
                |> List.map (treeNodeAnswer appState props model (isSelf || isChild))

        itemTemplateQuestions =
            KnowledgeModel.getQuestionItemTemplateQuestions uuid props.editorContext.kmEditor.knowledgeModel
                |> EditorContext.filterDeletedWith Question.getUuid props.editorContext
                |> List.map (treeNodeQuestion appState props model (isSelf || isChild))

        experts =
            KnowledgeModel.getQuestionExperts uuid props.editorContext.kmEditor.knowledgeModel
                |> List.filter (\e -> e.uuid == props.movingUuid)
                |> List.map (treeNodeExpert appState props)

        references =
            KnowledgeModel.getQuestionReferences uuid props.editorContext.kmEditor.knowledgeModel
                |> List.filter (\r -> Reference.getUuid r == props.movingUuid)
                |> List.map (treeNodeReference appState props)

        config =
            { uuid = uuid
            , icon = faKmQuestion
            , label = Question.getTitle question
            , children = answers ++ itemTemplateQuestions ++ experts ++ references
            , untitledLabel = gettext "Untitled question" appState.locale
            , allowed = allowed
            , open = isTreeOpen uuid model
            , selected = isSelected uuid model
            , current = isSelf
            }
    in
    treeNode config


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
            KnowledgeModel.getAnswerFollowupQuestions answer.uuid props.editorContext.kmEditor.knowledgeModel
                |> EditorContext.filterDeletedWith Question.getUuid props.editorContext
                |> List.map (treeNodeQuestion appState props model (isSelf || isChild))

        config =
            { uuid = answer.uuid
            , icon = faKmAnswer
            , label = answer.label
            , children = followupQuestions
            , untitledLabel = gettext "Untitled answer" appState.locale
            , allowed = allowed
            , open = isTreeOpen answer.uuid model
            , selected = isSelected answer.uuid model
            , current = isSelf
            }
    in
    treeNode config


treeNodeExpert : AppState -> ViewProps -> Expert -> Html Msg
treeNodeExpert appState props expert =
    let
        isSelf =
            props.movingUuid == expert.uuid

        config =
            { uuid = expert.uuid
            , icon = faKmExpert
            , label = expert.name
            , children = []
            , untitledLabel = gettext "Untitled expert" appState.locale
            , allowed = False
            , open = False
            , selected = False
            , current = isSelf
            }
    in
    treeNode config


treeNodeReference : AppState -> ViewProps -> Reference -> Html Msg
treeNodeReference appState props reference =
    let
        isSelf =
            props.movingUuid == Reference.getUuid reference

        config =
            { uuid = Reference.getUuid reference
            , icon = faKmReference
            , label = Reference.getVisibleName (KnowledgeModel.getAllQuestions props.editorContext.kmEditor.knowledgeModel) (KnowledgeModel.getAllResourcePages props.editorContext.kmEditor.knowledgeModel) reference
            , children = []
            , untitledLabel = gettext "Untitled reference" appState.locale
            , allowed = False
            , open = False
            , selected = False
            , current = isSelf
            }
    in
    treeNode config


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


treeNode : TreeNodeConfig Msg -> Html Msg
treeNode config =
    let
        caret =
            if List.isEmpty config.children then
                Html.nothing

            else
                treeNodeCaret (ToggleTreeOpen config.uuid) config.open

        children =
            if config.open then
                ul [] config.children

            else
                Html.nothing

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


treeNodeCaret : Msg -> Bool -> Html Msg
treeNodeCaret msg isOpen =
    a [ class "caret", onClick msg, dataCy "km-editor_move-modal_item_caret" ]
        [ Html.viewIf (not isOpen) faKmEditorTreeClosed
        , Html.viewIf isOpen faKmEditorTreeOpened
        ]
