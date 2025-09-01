module Wizard.Pages.KMEditor.Editor.Components.KMEditor.Tree exposing
    ( CreateEvents
    , ViewProps
    , view
    )

import Flip exposing (flip)
import Gettext exposing (gettext)
import Html exposing (Html, a, div, li, span, text, ul)
import Html.Attributes exposing (attribute, class, classList)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Shared.Components.FontAwesome exposing (fa, faKmAnswer, faKmChapter, faKmChoice, faKmEditorCollapseAll, faKmEditorExpandAll, faKmEditorTreeClosed, faKmEditorTreeOpened, faKmExpert, faKmIntegration, faKmKnowledgeModel, faKmMetric, faKmPhase, faKmQuestion, faKmReference, faKmResourceCollection, faKmResourcePage, faKmTag)
import Uuid
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel
import Wizard.Api.Models.KnowledgeModel.Answer exposing (Answer)
import Wizard.Api.Models.KnowledgeModel.Chapter exposing (Chapter)
import Wizard.Api.Models.KnowledgeModel.Choice exposing (Choice)
import Wizard.Api.Models.KnowledgeModel.Expert as Expert exposing (Expert)
import Wizard.Api.Models.KnowledgeModel.Integration as Integration exposing (Integration)
import Wizard.Api.Models.KnowledgeModel.Metric exposing (Metric)
import Wizard.Api.Models.KnowledgeModel.Phase exposing (Phase)
import Wizard.Api.Models.KnowledgeModel.Question as Question exposing (Question)
import Wizard.Api.Models.KnowledgeModel.Reference as Reference exposing (Reference)
import Wizard.Api.Models.KnowledgeModel.ResourceCollection exposing (ResourceCollection)
import Wizard.Api.Models.KnowledgeModel.ResourcePage exposing (ResourcePage)
import Wizard.Api.Models.KnowledgeModel.Tag exposing (Tag)
import Wizard.Components.Html exposing (linkTo)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KMEditor.Editor.Common.EditorBranch as EditorBranch exposing (EditorBranch)
import Wizard.Routes as Routes


type alias ViewProps msg =
    { expandAll : msg
    , collapseAll : msg
    , setTreeOpen : String -> Bool -> msg
    , createEvents : CreateEvents msg
    }


type alias CreateEvents msg =
    { createChapter : String -> msg
    , createQuestion : String -> msg
    , createAnswer : String -> msg
    , createChoice : String -> msg
    , createExpert : String -> msg
    , createReference : String -> msg
    , createResourceCollection : String -> msg
    , createResourcePage : String -> msg
    , createIntegration : String -> msg
    , createTag : String -> msg
    , createMetric : String -> msg
    , createPhase : String -> msg
    }


view : ViewProps msg -> AppState -> EditorBranch -> Html msg
view props appState editorBranch =
    div [ class "tree-col" ]
        [ div [ class "diff-tree" ]
            [ div [ class "inner" ]
                [ div [ class "actions" ]
                    [ a [ onClick props.expandAll ]
                        [ faKmEditorExpandAll
                        , text (gettext "Expand all" appState.locale)
                        ]
                    , a [ onClick props.collapseAll ]
                        [ faKmEditorCollapseAll
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

        chapterNodes =
            List.map (treeNodeChapter props appState editorBranch) chapters

        addChapter =
            treeNodeAdd (anyEntityActive editorBranch (List.map .uuid chapters))
                (props.createEvents.createChapter uuid)
                (gettext "Add chapter" appState.locale)

        metrics =
            KnowledgeModel.getMetrics knowledgeModel
                |> EditorBranch.sortDeleted .uuid editorBranch

        metricNodes =
            List.map (treeNodeMetric props appState editorBranch) metrics

        addMetric =
            treeNodeAdd (anyEntityActive editorBranch (List.map .uuid metrics))
                (props.createEvents.createMetric uuid)
                (gettext "Add metric" appState.locale)

        phases =
            KnowledgeModel.getPhases knowledgeModel
                |> EditorBranch.sortDeleted .uuid editorBranch

        phaseNodes =
            List.map (treeNodePhase props appState editorBranch) phases

        addPhase =
            treeNodeAdd (anyEntityActive editorBranch (List.map .uuid phases))
                (props.createEvents.createPhase uuid)
                (gettext "Add phase" appState.locale)

        tags =
            KnowledgeModel.getTags knowledgeModel
                |> EditorBranch.sortDeleted .uuid editorBranch

        tagNodes =
            List.map (treeNodeTag props appState editorBranch) tags

        addTag =
            treeNodeAdd (anyEntityActive editorBranch (List.map .uuid tags))
                (props.createEvents.createTag uuid)
                (gettext "Add tag" appState.locale)

        integrations =
            KnowledgeModel.getIntegrations knowledgeModel
                |> EditorBranch.sortDeleted Integration.getUuid editorBranch

        integrationNodes =
            List.map (treeNodeIntegration props appState editorBranch) integrations

        addIntegration =
            treeNodeAdd (anyEntityActive editorBranch (List.map Integration.getUuid integrations))
                (props.createEvents.createIntegration uuid)
                (gettext "Add integration" appState.locale)

        resourceCollections =
            KnowledgeModel.getResourceCollections knowledgeModel
                |> EditorBranch.sortDeleted .uuid editorBranch

        resourceCollectionNodes =
            List.map (treeNodeResourceCollection props appState editorBranch) resourceCollections

        addResourceCollection =
            treeNodeAdd (anyEntityActive editorBranch (List.map .uuid resourceCollections))
                (props.createEvents.createResourceCollection uuid)
                (gettext "Add resource collection" appState.locale)

        config =
            { uuid = uuid
            , icon = faKmKnowledgeModel
            , label = editorBranch.branch.name
            , children =
                List.concat
                    [ chapterNodes
                    , addChapter
                    , metricNodes
                    , addMetric
                    , phaseNodes
                    , addPhase
                    , tagNodes
                    , addTag
                    , integrationNodes
                    , addIntegration
                    , resourceCollectionNodes
                    , addResourceCollection
                    ]
            , untitledLabel = ""
            }
    in
    treeNode props editorBranch config


treeNodeChapter : ViewProps msg -> AppState -> EditorBranch -> Chapter -> Html msg
treeNodeChapter props appState editorBranch chapter =
    let
        questions =
            KnowledgeModel.getChapterQuestions chapter.uuid editorBranch.branch.knowledgeModel
                |> EditorBranch.sortDeleted Question.getUuid editorBranch

        questionNodes =
            List.map (treeNodeQuestion props appState editorBranch) questions

        addQuestion =
            treeNodeAdd (anyEntityActive editorBranch (List.map Question.getUuid questions))
                (props.createEvents.createQuestion chapter.uuid)
                (gettext "Add question" appState.locale)

        config =
            { uuid = chapter.uuid
            , icon = faKmChapter
            , label = chapter.title
            , children = questionNodes ++ addQuestion
            , untitledLabel = gettext "Untitled chapter" appState.locale
            }
    in
    treeNode props editorBranch config


treeNodeMetric : ViewProps msg -> AppState -> EditorBranch -> Metric -> Html msg
treeNodeMetric props appState editorBranch metric =
    let
        config =
            { uuid = metric.uuid
            , icon = faKmMetric
            , label = metric.title
            , children = []
            , untitledLabel = gettext "Untitled metric" appState.locale
            }
    in
    treeNode props editorBranch config


treeNodePhase : ViewProps msg -> AppState -> EditorBranch -> Phase -> Html msg
treeNodePhase props appState editorBranch phase =
    let
        config =
            { uuid = phase.uuid
            , icon = faKmPhase
            , label = phase.title
            , children = []
            , untitledLabel = gettext "Untitled phase" appState.locale
            }
    in
    treeNode props editorBranch config


treeNodeTag : ViewProps msg -> AppState -> EditorBranch -> Tag -> Html msg
treeNodeTag props appState editorBranch tag =
    let
        config =
            { uuid = tag.uuid
            , icon = faKmTag
            , label = tag.name
            , children = []
            , untitledLabel = gettext "Untitled tag" appState.locale
            }
    in
    treeNode props editorBranch config


treeNodeIntegration : ViewProps msg -> AppState -> EditorBranch -> Integration -> Html msg
treeNodeIntegration props appState editorBranch integration =
    let
        config =
            { uuid = Integration.getUuid integration
            , icon = faKmIntegration
            , label = Integration.getVisibleName integration
            , children = []
            , untitledLabel = gettext "Untitled integration" appState.locale
            }
    in
    treeNode props editorBranch config


treeNodeQuestion : ViewProps msg -> AppState -> EditorBranch -> Question -> Html msg
treeNodeQuestion props appState editorBranch question =
    let
        uuid =
            Question.getUuid question

        answers =
            KnowledgeModel.getQuestionAnswers uuid editorBranch.branch.knowledgeModel
                |> EditorBranch.sortDeleted .uuid editorBranch

        answerNodes =
            List.map (treeNodeAnswer props appState editorBranch) answers

        addAnswer =
            treeNodeAdd (anyEntityActive editorBranch (List.map .uuid answers))
                (props.createEvents.createAnswer uuid)
                (gettext "Add answer" appState.locale)

        itemTemplateQuestions =
            KnowledgeModel.getQuestionItemTemplateQuestions uuid editorBranch.branch.knowledgeModel
                |> EditorBranch.sortDeleted Question.getUuid editorBranch

        itemTemplateQuestionNodes =
            List.map (treeNodeQuestion props appState editorBranch) itemTemplateQuestions

        addItemTemplateQuestion =
            treeNodeAdd (anyEntityActive editorBranch (List.map Question.getUuid itemTemplateQuestions))
                (props.createEvents.createQuestion uuid)
                (gettext "Add question" appState.locale)

        choices =
            KnowledgeModel.getQuestionChoices uuid editorBranch.branch.knowledgeModel
                |> EditorBranch.sortDeleted .uuid editorBranch

        choiceNodes =
            List.map (treeNodeChoice props appState editorBranch) choices

        addChoice =
            treeNodeAdd (anyEntityActive editorBranch (List.map .uuid choices))
                (props.createEvents.createChoice uuid)
                (gettext "Add choice" appState.locale)

        references =
            KnowledgeModel.getQuestionReferences uuid editorBranch.branch.knowledgeModel
                |> EditorBranch.sortDeleted Reference.getUuid editorBranch

        referenceNodes =
            List.map (treeNodeReference props appState editorBranch) references

        addReference =
            treeNodeAdd (anyEntityActive editorBranch (List.map Reference.getUuid references))
                (props.createEvents.createReference uuid)
                (gettext "Add reference" appState.locale)

        experts =
            KnowledgeModel.getQuestionExperts uuid editorBranch.branch.knowledgeModel
                |> EditorBranch.sortDeleted .uuid editorBranch

        expertNodes =
            List.map (treeNodeExperts props appState editorBranch) experts

        addExpert =
            treeNodeAdd (anyEntityActive editorBranch (List.map .uuid experts))
                (props.createEvents.createExpert uuid)
                (gettext "Add expert" appState.locale)

        config =
            { uuid = uuid
            , icon = faKmQuestion
            , label = Question.getTitle question
            , children =
                List.concat
                    [ answerNodes
                    , addAnswer
                    , itemTemplateQuestionNodes
                    , addItemTemplateQuestion
                    , choiceNodes
                    , addChoice
                    , referenceNodes
                    , addReference
                    , expertNodes
                    , addExpert
                    ]
            , untitledLabel = gettext "Untitled question" appState.locale
            }
    in
    treeNode props editorBranch config


treeNodeAnswer : ViewProps msg -> AppState -> EditorBranch -> Answer -> Html msg
treeNodeAnswer props appState editorBranch answer =
    let
        followupQuestions =
            KnowledgeModel.getAnswerFollowupQuestions answer.uuid editorBranch.branch.knowledgeModel
                |> EditorBranch.sortDeleted Question.getUuid editorBranch

        followupQuestionNodes =
            List.map (treeNodeQuestion props appState editorBranch) followupQuestions

        addFollowupQuestion =
            treeNodeAdd (anyEntityActive editorBranch (List.map Question.getUuid followupQuestions))
                (props.createEvents.createQuestion answer.uuid)
                (gettext "Add question" appState.locale)

        config =
            { uuid = answer.uuid
            , icon = faKmAnswer
            , label = answer.label
            , children = followupQuestionNodes ++ addFollowupQuestion
            , untitledLabel = gettext "Untitled answer" appState.locale
            }
    in
    treeNode props editorBranch config


treeNodeChoice : ViewProps msg -> AppState -> EditorBranch -> Choice -> Html msg
treeNodeChoice props appState editorBranch choice =
    let
        config =
            { uuid = choice.uuid
            , icon = faKmChoice
            , label = choice.label
            , children = []
            , untitledLabel = gettext "Untitled choice" appState.locale
            }
    in
    treeNode props editorBranch config


treeNodeReference : ViewProps msg -> AppState -> EditorBranch -> Reference -> Html msg
treeNodeReference props appState editorBranch reference =
    let
        config =
            { uuid = Reference.getUuid reference
            , icon = faKmReference
            , label = Reference.getVisibleName (KnowledgeModel.getAllResourcePages editorBranch.branch.knowledgeModel) reference
            , children = []
            , untitledLabel = gettext "Untitled reference" appState.locale
            }
    in
    treeNode props editorBranch config


treeNodeExperts : ViewProps msg -> AppState -> EditorBranch -> Expert -> Html msg
treeNodeExperts props appState editorBranch expert =
    let
        config =
            { uuid = expert.uuid
            , icon = faKmExpert
            , label = Expert.getVisibleName expert
            , children = []
            , untitledLabel = gettext "Untitled expert" appState.locale
            }
    in
    treeNode props editorBranch config


treeNodeResourceCollection : ViewProps msg -> AppState -> EditorBranch -> ResourceCollection -> Html msg
treeNodeResourceCollection props appState editorBranch resourceCollection =
    let
        resourcePages =
            KnowledgeModel.getResourceCollectionResourcePages resourceCollection.uuid editorBranch.branch.knowledgeModel
                |> EditorBranch.sortDeleted .uuid editorBranch

        resourcePageNodes =
            List.map (treeNodeResourcePage props appState editorBranch) resourcePages

        addResourcePage =
            treeNodeAdd (anyEntityActive editorBranch (List.map .uuid resourcePages))
                (props.createEvents.createResourcePage resourceCollection.uuid)
                (gettext "Add resource page" appState.locale)

        config =
            { uuid = resourceCollection.uuid
            , icon = faKmResourceCollection
            , label = resourceCollection.title
            , children = resourcePageNodes ++ addResourcePage
            , untitledLabel = gettext "Untitled resource collection" appState.locale
            }
    in
    treeNode props editorBranch config


treeNodeResourcePage : ViewProps msg -> AppState -> EditorBranch -> ResourcePage -> Html msg
treeNodeResourcePage props appState editorBranch resourcePage =
    let
        config =
            { uuid = resourcePage.uuid
            , icon = faKmResourcePage
            , label = resourcePage.title
            , children = []
            , untitledLabel = gettext "Untitled resource page" appState.locale
            }
    in
    treeNode props editorBranch config


type alias TreeNodeConfig msg =
    { uuid : String
    , icon : Html msg
    , label : String
    , children : List (Html msg)
    , untitledLabel : String
    }


treeNode : ViewProps msg -> EditorBranch -> TreeNodeConfig msg -> Html msg
treeNode props editorBranch config =
    let
        ( caret, children ) =
            if EditorBranch.isDeleted config.uuid editorBranch || List.isEmpty config.children then
                ( Html.nothing, Html.nothing )

            else if EditorBranch.treeIsNodeOpen config.uuid editorBranch then
                ( treeNodeCaret (props.setTreeOpen config.uuid) True
                , ul [] config.children
                )

            else
                ( treeNodeCaret (props.setTreeOpen config.uuid) False
                , Html.nothing
                )

        link =
            if EditorBranch.isDeleted config.uuid editorBranch then
                a []

            else
                linkTo (Routes.kmEditorEditor editorBranch.branch.uuid (EditorBranch.getEditUuid config.uuid editorBranch))
                    [ dataCy "km-editor_tree_link"
                    , attribute "data-km-editor-link" config.uuid
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


treeNodeCaret : (Bool -> msg) -> Bool -> Html msg
treeNodeCaret toggleMsg isOpen =
    a [ class "caret", onClick (toggleMsg (not isOpen)) ]
        [ Html.viewIf (not isOpen) faKmEditorTreeClosed
        , Html.viewIf isOpen faKmEditorTreeOpened
        ]


treeNodeAdd : Bool -> msg -> String -> List (Html msg)
treeNodeAdd isVisible addMsg addLabel =
    if isVisible then
        [ li [ class "add-entity" ]
            [ a [ onClick addMsg ] [ fa "fas fa-plus", text addLabel ]
            ]
        ]

    else
        []


anyEntityActive : EditorBranch -> List String -> Bool
anyEntityActive editorBranch =
    List.any (flip EditorBranch.isActive editorBranch)
