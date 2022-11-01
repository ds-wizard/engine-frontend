module Wizard.KMEditor.Editor.Components.KMEditor.Tree exposing
    ( ViewProps
    , view
    )

import Gettext exposing (gettext)
import Html exposing (Html, a, div, i, li, span, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Shared.Data.KnowledgeModel as KnowledgeModel
import Shared.Data.KnowledgeModel.Answer exposing (Answer)
import Shared.Data.KnowledgeModel.Chapter exposing (Chapter)
import Shared.Data.KnowledgeModel.Choice exposing (Choice)
import Shared.Data.KnowledgeModel.Expert as Expert exposing (Expert)
import Shared.Data.KnowledgeModel.Integration as Integration exposing (Integration)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.KnowledgeModel.Phase exposing (Phase)
import Shared.Data.KnowledgeModel.Question as Question exposing (Question)
import Shared.Data.KnowledgeModel.Reference as Reference exposing (Reference)
import Shared.Data.KnowledgeModel.Tag exposing (Tag)
import Shared.Html exposing (emptyNode, faKeyClass, faSet)
import Uuid
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.KMEditor.Editor.Common.EditorBranch as EditorBranch exposing (EditorBranch)
import Wizard.Routes as Routes


type alias ViewProps msg =
    { expandAll : msg
    , collapseAll : msg
    , setTreeOpen : String -> Bool -> msg
    }


view : ViewProps msg -> AppState -> EditorBranch -> Html msg
view props appState editorBranch =
    div [ class "tree-col" ]
        [ div [ class "diff-tree" ]
            [ div [ class "inner" ]
                [ div [ class "actions" ]
                    [ a [ onClick props.expandAll ]
                        [ faSet "kmEditor.expandAll" appState
                        , text (gettext "Expand all" appState.locale)
                        ]
                    , a [ onClick props.collapseAll ]
                        [ faSet "kmEditor.collapseAll" appState
                        , text (gettext "Collapse all" appState.locale)
                        ]
                    ]
                , ul [] [ treeNodeKM props appState editorBranch ]
                ]
            ]
        ]


treeNodeKM : ViewProps msg -> AppState -> EditorBranch -> Html msg
treeNodeKM props appState editorBranch =
    let
        knowledgeModel =
            editorBranch.branch.knowledgeModel

        uuid =
            Uuid.toString knowledgeModel.uuid

        chapters =
            KnowledgeModel.getChapters knowledgeModel
                |> EditorBranch.sortDeleted .uuid editorBranch
                |> List.map (treeNodeChapter props appState editorBranch)

        metrics =
            KnowledgeModel.getMetrics knowledgeModel
                |> EditorBranch.sortDeleted .uuid editorBranch
                |> List.map (treeNodeMetric props appState editorBranch)

        phases =
            KnowledgeModel.getPhases knowledgeModel
                |> EditorBranch.sortDeleted .uuid editorBranch
                |> List.map (treeNodePhase props appState editorBranch)

        tags =
            KnowledgeModel.getTags knowledgeModel
                |> EditorBranch.sortDeleted .uuid editorBranch
                |> List.map (treeNodeTag props appState editorBranch)

        integrations =
            KnowledgeModel.getIntegrations knowledgeModel
                |> EditorBranch.sortDeleted Integration.getUuid editorBranch
                |> List.map (treeNodeIntegration props appState editorBranch)

        config =
            { uuid = uuid
            , icon = faSet "km.knowledgeModel" appState
            , label = editorBranch.branch.name
            , children = chapters ++ metrics ++ phases ++ tags ++ integrations
            , untitledLabel = ""
            }
    in
    treeNode props appState editorBranch config


treeNodeChapter : ViewProps msg -> AppState -> EditorBranch -> Chapter -> Html msg
treeNodeChapter props appState editorBranch chapter =
    let
        questions =
            KnowledgeModel.getChapterQuestions chapter.uuid editorBranch.branch.knowledgeModel
                |> EditorBranch.sortDeleted Question.getUuid editorBranch
                |> List.map (treeNodeQuestion props appState editorBranch)

        config =
            { uuid = chapter.uuid
            , icon = faSet "km.chapter" appState
            , label = chapter.title
            , children = questions
            , untitledLabel = gettext "Untitled chapter" appState.locale
            }
    in
    treeNode props appState editorBranch config


treeNodeMetric : ViewProps msg -> AppState -> EditorBranch -> Metric -> Html msg
treeNodeMetric props appState editorBranch metric =
    let
        config =
            { uuid = metric.uuid
            , icon = faSet "km.metric" appState
            , label = metric.title
            , children = []
            , untitledLabel = gettext "Untitled metric" appState.locale
            }
    in
    treeNode props appState editorBranch config


treeNodePhase : ViewProps msg -> AppState -> EditorBranch -> Phase -> Html msg
treeNodePhase props appState editorBranch phase =
    let
        config =
            { uuid = phase.uuid
            , icon = faSet "km.phase" appState
            , label = phase.title
            , children = []
            , untitledLabel = gettext "Untitled phase" appState.locale
            }
    in
    treeNode props appState editorBranch config


treeNodeTag : ViewProps msg -> AppState -> EditorBranch -> Tag -> Html msg
treeNodeTag props appState editorBranch tag =
    let
        config =
            { uuid = tag.uuid
            , icon = faSet "km.tag" appState
            , label = tag.name
            , children = []
            , untitledLabel = gettext "Untitled tag" appState.locale
            }
    in
    treeNode props appState editorBranch config


treeNodeIntegration : ViewProps msg -> AppState -> EditorBranch -> Integration -> Html msg
treeNodeIntegration props appState editorBranch integration =
    let
        config =
            { uuid = Integration.getUuid integration
            , icon = faSet "km.integration" appState
            , label = Integration.getVisibleName integration
            , children = []
            , untitledLabel = gettext "Untitled integration" appState.locale
            }
    in
    treeNode props appState editorBranch config


treeNodeQuestion : ViewProps msg -> AppState -> EditorBranch -> Question -> Html msg
treeNodeQuestion props appState editorBranch question =
    let
        uuid =
            Question.getUuid question

        answers =
            KnowledgeModel.getQuestionAnswers uuid editorBranch.branch.knowledgeModel
                |> EditorBranch.sortDeleted .uuid editorBranch
                |> List.map (treeNodeAnswer props appState editorBranch)

        itemTemplateQuestions =
            KnowledgeModel.getQuestionItemTemplateQuestions uuid editorBranch.branch.knowledgeModel
                |> EditorBranch.sortDeleted Question.getUuid editorBranch
                |> List.map (treeNodeQuestion props appState editorBranch)

        choices =
            KnowledgeModel.getQuestionChoices uuid editorBranch.branch.knowledgeModel
                |> EditorBranch.sortDeleted .uuid editorBranch
                |> List.map (treeNodeChoice props appState editorBranch)

        references =
            KnowledgeModel.getQuestionReferences uuid editorBranch.branch.knowledgeModel
                |> EditorBranch.sortDeleted Reference.getUuid editorBranch
                |> List.map (treeNodeReference props appState editorBranch)

        experts =
            KnowledgeModel.getQuestionExperts uuid editorBranch.branch.knowledgeModel
                |> EditorBranch.sortDeleted .uuid editorBranch
                |> List.map (treeNodeExperts props appState editorBranch)

        config =
            { uuid = uuid
            , icon = faSet "km.question" appState
            , label = Question.getTitle question
            , children = answers ++ itemTemplateQuestions ++ choices ++ references ++ experts
            , untitledLabel = gettext "Untitled question" appState.locale
            }
    in
    treeNode props appState editorBranch config


treeNodeAnswer : ViewProps msg -> AppState -> EditorBranch -> Answer -> Html msg
treeNodeAnswer props appState editorBranch answer =
    let
        followupQuestions =
            KnowledgeModel.getAnswerFollowupQuestions answer.uuid editorBranch.branch.knowledgeModel
                |> EditorBranch.sortDeleted Question.getUuid editorBranch
                |> List.map (treeNodeQuestion props appState editorBranch)

        config =
            { uuid = answer.uuid
            , icon = faSet "km.answer" appState
            , label = answer.label
            , children = followupQuestions
            , untitledLabel = gettext "Untitled answer" appState.locale
            }
    in
    treeNode props appState editorBranch config


treeNodeChoice : ViewProps msg -> AppState -> EditorBranch -> Choice -> Html msg
treeNodeChoice props appState editorBranch choice =
    let
        config =
            { uuid = choice.uuid
            , icon = faSet "km.choice" appState
            , label = choice.label
            , children = []
            , untitledLabel = gettext "Untitled choice" appState.locale
            }
    in
    treeNode props appState editorBranch config


treeNodeReference : ViewProps msg -> AppState -> EditorBranch -> Reference -> Html msg
treeNodeReference props appState editorBranch reference =
    let
        config =
            { uuid = Reference.getUuid reference
            , icon = faSet "km.reference" appState
            , label = Reference.getVisibleName reference
            , children = []
            , untitledLabel = gettext "Untitled reference" appState.locale
            }
    in
    treeNode props appState editorBranch config


treeNodeExperts : ViewProps msg -> AppState -> EditorBranch -> Expert -> Html msg
treeNodeExperts props appState editorBranch expert =
    let
        config =
            { uuid = expert.uuid
            , icon = faSet "km.expert" appState
            , label = Expert.getVisibleName expert
            , children = []
            , untitledLabel = gettext "Untitled expert" appState.locale
            }
    in
    treeNode props appState editorBranch config


type alias TreeNodeConfig msg =
    { uuid : String
    , icon : Html msg
    , label : String
    , children : List (Html msg)
    , untitledLabel : String
    }


treeNode : ViewProps msg -> AppState -> EditorBranch -> TreeNodeConfig msg -> Html msg
treeNode props appState editorBranch config =
    let
        ( caret, children ) =
            if EditorBranch.isDeleted config.uuid editorBranch || List.isEmpty config.children then
                ( emptyNode, emptyNode )

            else if EditorBranch.treeIsNodeOpen config.uuid editorBranch then
                ( treeNodeCaret appState (props.setTreeOpen config.uuid) True
                , ul [] config.children
                )

            else
                ( treeNodeCaret appState (props.setTreeOpen config.uuid) False
                , emptyNode
                )

        link =
            if EditorBranch.isDeleted config.uuid editorBranch then
                a []

            else
                linkTo appState
                    (Routes.kmEditorEditor editorBranch.branch.uuid (EditorBranch.getEditUuid config.uuid editorBranch))
                    [ dataCy "km-editor_tree_link"
                    ]

        ( untitled, visibleLabel ) =
            if String.isEmpty config.label then
                ( True, config.untitledLabel )

            else
                ( False, config.label )
    in
    li
        [ classList
            [ ( "active", EditorBranch.isActive config.uuid editorBranch )
            , ( "state-edited", EditorBranch.isEdited config.uuid editorBranch )
            , ( "state-deleted", EditorBranch.isDeleted config.uuid editorBranch )
            , ( "state-added", EditorBranch.isAdded config.uuid editorBranch )
            ]
        ]
        [ caret
        , link
            [ config.icon
            , span [ classList [ ( "untitled", untitled ) ] ] [ text visibleLabel ]
            ]
        , children
        ]


treeNodeCaret : AppState -> (Bool -> msg) -> Bool -> Html msg
treeNodeCaret appState toggleMsg isOpen =
    a [ class "caret", onClick (toggleMsg (not isOpen)) ]
        [ i
            [ classList
                [ ( faKeyClass "kmEditor.treeClosed" appState, not isOpen )
                , ( faKeyClass "kmEditor.treeOpened" appState, isOpen )
                ]
            ]
            []
        ]
