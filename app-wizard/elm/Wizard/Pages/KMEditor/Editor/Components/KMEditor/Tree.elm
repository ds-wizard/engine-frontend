module Wizard.Pages.KMEditor.Editor.Components.KMEditor.Tree exposing
    ( CreateEvents
    , ViewProps
    , view
    )

import Common.Components.FontAwesome exposing (fa, faKmAnswer, faKmChapter, faKmChoice, faKmEditorCollapseAll, faKmEditorExpandAll, faKmEditorTreeClosed, faKmEditorTreeOpened, faKmExpert, faKmIntegration, faKmKnowledgeModel, faKmMetric, faKmPhase, faKmQuestion, faKmReference, faKmResourceCollection, faKmResourcePage, faKmTag)
import Flip exposing (flip)
import Gettext exposing (gettext)
import Html exposing (Html, a, div, li, span, text, ul)
import Html.Attributes exposing (attribute, class, classList)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick)
import Html.Extra as Html
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
import Wizard.Pages.KMEditor.Editor.Common.EditorContext as EditorContext exposing (EditorContext)
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


view : ViewProps msg -> AppState -> EditorContext -> Html msg
view props appState editorContext =
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
                , ul [] [ treeNodeKM props appState editorContext ]
                ]
            ]
        ]


treeNodeKM : ViewProps msg -> AppState -> EditorContext -> Html msg
treeNodeKM props appState editorContext =
    let
        knowledgeModel =
            editorContext.kmEditor.knowledgeModel

        uuid =
            Uuid.toString knowledgeModel.uuid

        chapters =
            KnowledgeModel.getChapters knowledgeModel
                |> EditorContext.sortDeleted .uuid editorContext

        chapterNodes =
            List.map (treeNodeChapter props appState editorContext) chapters

        addChapter =
            treeNodeAdd (anyEntityActive editorContext (List.map .uuid chapters))
                (props.createEvents.createChapter uuid)
                (gettext "Add chapter" appState.locale)

        metrics =
            KnowledgeModel.getMetrics knowledgeModel
                |> EditorContext.sortDeleted .uuid editorContext

        metricNodes =
            List.map (treeNodeMetric props appState editorContext) metrics

        addMetric =
            treeNodeAdd (anyEntityActive editorContext (List.map .uuid metrics))
                (props.createEvents.createMetric uuid)
                (gettext "Add metric" appState.locale)

        phases =
            KnowledgeModel.getPhases knowledgeModel
                |> EditorContext.sortDeleted .uuid editorContext

        phaseNodes =
            List.map (treeNodePhase props appState editorContext) phases

        addPhase =
            treeNodeAdd (anyEntityActive editorContext (List.map .uuid phases))
                (props.createEvents.createPhase uuid)
                (gettext "Add phase" appState.locale)

        tags =
            KnowledgeModel.getTags knowledgeModel
                |> EditorContext.sortDeleted .uuid editorContext

        tagNodes =
            List.map (treeNodeTag props appState editorContext) tags

        addTag =
            treeNodeAdd (anyEntityActive editorContext (List.map .uuid tags))
                (props.createEvents.createTag uuid)
                (gettext "Add tag" appState.locale)

        integrations =
            KnowledgeModel.getIntegrations knowledgeModel
                |> EditorContext.sortDeleted Integration.getUuid editorContext

        integrationNodes =
            List.map (treeNodeIntegration props appState editorContext) integrations

        addIntegration =
            treeNodeAdd (anyEntityActive editorContext (List.map Integration.getUuid integrations))
                (props.createEvents.createIntegration uuid)
                (gettext "Add integration" appState.locale)

        resourceCollections =
            KnowledgeModel.getResourceCollections knowledgeModel
                |> EditorContext.sortDeleted .uuid editorContext

        resourceCollectionNodes =
            List.map (treeNodeResourceCollection props appState editorContext) resourceCollections

        addResourceCollection =
            treeNodeAdd (anyEntityActive editorContext (List.map .uuid resourceCollections))
                (props.createEvents.createResourceCollection uuid)
                (gettext "Add resource collection" appState.locale)

        config =
            { uuid = uuid
            , icon = faKmKnowledgeModel
            , label = editorContext.kmEditor.name
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
    treeNode props editorContext config


treeNodeChapter : ViewProps msg -> AppState -> EditorContext -> Chapter -> Html msg
treeNodeChapter props appState editorContext chapter =
    let
        questions =
            KnowledgeModel.getChapterQuestions chapter.uuid editorContext.kmEditor.knowledgeModel
                |> EditorContext.sortDeleted Question.getUuid editorContext

        questionNodes =
            List.map (treeNodeQuestion props appState editorContext) questions

        addQuestion =
            treeNodeAdd (anyEntityActive editorContext (List.map Question.getUuid questions))
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
    treeNode props editorContext config


treeNodeMetric : ViewProps msg -> AppState -> EditorContext -> Metric -> Html msg
treeNodeMetric props appState editorContext metric =
    let
        config =
            { uuid = metric.uuid
            , icon = faKmMetric
            , label = metric.title
            , children = []
            , untitledLabel = gettext "Untitled metric" appState.locale
            }
    in
    treeNode props editorContext config


treeNodePhase : ViewProps msg -> AppState -> EditorContext -> Phase -> Html msg
treeNodePhase props appState editorContext phase =
    let
        config =
            { uuid = phase.uuid
            , icon = faKmPhase
            , label = phase.title
            , children = []
            , untitledLabel = gettext "Untitled phase" appState.locale
            }
    in
    treeNode props editorContext config


treeNodeTag : ViewProps msg -> AppState -> EditorContext -> Tag -> Html msg
treeNodeTag props appState editorContext tag =
    let
        config =
            { uuid = tag.uuid
            , icon = faKmTag
            , label = tag.name
            , children = []
            , untitledLabel = gettext "Untitled tag" appState.locale
            }
    in
    treeNode props editorContext config


treeNodeIntegration : ViewProps msg -> AppState -> EditorContext -> Integration -> Html msg
treeNodeIntegration props appState editorContext integration =
    let
        config =
            { uuid = Integration.getUuid integration
            , icon = faKmIntegration
            , label = Integration.getVisibleName integration
            , children = []
            , untitledLabel = gettext "Untitled integration" appState.locale
            }
    in
    treeNode props editorContext config


treeNodeQuestion : ViewProps msg -> AppState -> EditorContext -> Question -> Html msg
treeNodeQuestion props appState editorContext question =
    let
        uuid =
            Question.getUuid question

        answers =
            KnowledgeModel.getQuestionAnswers uuid editorContext.kmEditor.knowledgeModel
                |> EditorContext.sortDeleted .uuid editorContext

        answerNodes =
            List.map (treeNodeAnswer props appState editorContext) answers

        addAnswer =
            treeNodeAdd (anyEntityActive editorContext (List.map .uuid answers))
                (props.createEvents.createAnswer uuid)
                (gettext "Add answer" appState.locale)

        itemTemplateQuestions =
            KnowledgeModel.getQuestionItemTemplateQuestions uuid editorContext.kmEditor.knowledgeModel
                |> EditorContext.sortDeleted Question.getUuid editorContext

        itemTemplateQuestionNodes =
            List.map (treeNodeQuestion props appState editorContext) itemTemplateQuestions

        addItemTemplateQuestion =
            treeNodeAdd (anyEntityActive editorContext (List.map Question.getUuid itemTemplateQuestions))
                (props.createEvents.createQuestion uuid)
                (gettext "Add question" appState.locale)

        choices =
            KnowledgeModel.getQuestionChoices uuid editorContext.kmEditor.knowledgeModel
                |> EditorContext.sortDeleted .uuid editorContext

        choiceNodes =
            List.map (treeNodeChoice props appState editorContext) choices

        addChoice =
            treeNodeAdd (anyEntityActive editorContext (List.map .uuid choices))
                (props.createEvents.createChoice uuid)
                (gettext "Add choice" appState.locale)

        references =
            KnowledgeModel.getQuestionReferences uuid editorContext.kmEditor.knowledgeModel
                |> EditorContext.sortDeleted Reference.getUuid editorContext

        referenceNodes =
            List.map (treeNodeReference props appState editorContext) references

        addReference =
            treeNodeAdd (anyEntityActive editorContext (List.map Reference.getUuid references))
                (props.createEvents.createReference uuid)
                (gettext "Add reference" appState.locale)

        experts =
            KnowledgeModel.getQuestionExperts uuid editorContext.kmEditor.knowledgeModel
                |> EditorContext.sortDeleted .uuid editorContext

        expertNodes =
            List.map (treeNodeExperts props appState editorContext) experts

        addExpert =
            treeNodeAdd (anyEntityActive editorContext (List.map .uuid experts))
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
    treeNode props editorContext config


treeNodeAnswer : ViewProps msg -> AppState -> EditorContext -> Answer -> Html msg
treeNodeAnswer props appState editorContext answer =
    let
        followupQuestions =
            KnowledgeModel.getAnswerFollowupQuestions answer.uuid editorContext.kmEditor.knowledgeModel
                |> EditorContext.sortDeleted Question.getUuid editorContext

        followupQuestionNodes =
            List.map (treeNodeQuestion props appState editorContext) followupQuestions

        addFollowupQuestion =
            treeNodeAdd (anyEntityActive editorContext (List.map Question.getUuid followupQuestions))
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
    treeNode props editorContext config


treeNodeChoice : ViewProps msg -> AppState -> EditorContext -> Choice -> Html msg
treeNodeChoice props appState editorContext choice =
    let
        config =
            { uuid = choice.uuid
            , icon = faKmChoice
            , label = choice.label
            , children = []
            , untitledLabel = gettext "Untitled choice" appState.locale
            }
    in
    treeNode props editorContext config


treeNodeReference : ViewProps msg -> AppState -> EditorContext -> Reference -> Html msg
treeNodeReference props appState editorContext reference =
    let
        config =
            { uuid = Reference.getUuid reference
            , icon = faKmReference
            , label = Reference.getVisibleName (KnowledgeModel.getAllQuestions editorContext.kmEditor.knowledgeModel) (KnowledgeModel.getAllResourcePages editorContext.kmEditor.knowledgeModel) reference
            , children = []
            , untitledLabel = gettext "Untitled reference" appState.locale
            }
    in
    treeNode props editorContext config


treeNodeExperts : ViewProps msg -> AppState -> EditorContext -> Expert -> Html msg
treeNodeExperts props appState editorContext expert =
    let
        config =
            { uuid = expert.uuid
            , icon = faKmExpert
            , label = Expert.getVisibleName expert
            , children = []
            , untitledLabel = gettext "Untitled expert" appState.locale
            }
    in
    treeNode props editorContext config


treeNodeResourceCollection : ViewProps msg -> AppState -> EditorContext -> ResourceCollection -> Html msg
treeNodeResourceCollection props appState editorContext resourceCollection =
    let
        resourcePages =
            KnowledgeModel.getResourceCollectionResourcePages resourceCollection.uuid editorContext.kmEditor.knowledgeModel
                |> EditorContext.sortDeleted .uuid editorContext

        resourcePageNodes =
            List.map (treeNodeResourcePage props appState editorContext) resourcePages

        addResourcePage =
            treeNodeAdd (anyEntityActive editorContext (List.map .uuid resourcePages))
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
    treeNode props editorContext config


treeNodeResourcePage : ViewProps msg -> AppState -> EditorContext -> ResourcePage -> Html msg
treeNodeResourcePage props appState editorContext resourcePage =
    let
        config =
            { uuid = resourcePage.uuid
            , icon = faKmResourcePage
            , label = resourcePage.title
            , children = []
            , untitledLabel = gettext "Untitled resource page" appState.locale
            }
    in
    treeNode props editorContext config


type alias TreeNodeConfig msg =
    { uuid : String
    , icon : Html msg
    , label : String
    , children : List (Html msg)
    , untitledLabel : String
    }


treeNode : ViewProps msg -> EditorContext -> TreeNodeConfig msg -> Html msg
treeNode props editorContext config =
    let
        ( caret, children ) =
            if EditorContext.isDeleted config.uuid editorContext || List.isEmpty config.children then
                ( Html.nothing, Html.nothing )

            else if EditorContext.treeIsNodeOpen config.uuid editorContext then
                ( treeNodeCaret (props.setTreeOpen config.uuid) True
                , ul [] config.children
                )

            else
                ( treeNodeCaret (props.setTreeOpen config.uuid) False
                , Html.nothing
                )

        link =
            if EditorContext.isDeleted config.uuid editorContext then
                a []

            else
                linkTo (Routes.kmEditorEditor editorContext.kmEditor.uuid (EditorContext.getEditUuid config.uuid editorContext))
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
            [ ( "active", EditorContext.isActive config.uuid editorContext )
            , ( "state-edited", EditorContext.isEdited config.uuid editorContext )
            , ( "state-deleted", EditorContext.isDeleted config.uuid editorContext )
            , ( "state-added", EditorContext.isAdded config.uuid editorContext )
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


anyEntityActive : EditorContext -> List String -> Bool
anyEntityActive editorContext =
    List.any (flip EditorContext.isActive editorContext)
