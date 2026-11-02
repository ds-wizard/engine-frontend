module Common.Components.Questionnaire.NodesStructureTest exposing (structureTest)

import Common.Api.Models.KnowledgeModel as KnowledgeModel
import Common.Api.Models.ProjectDetail.Reply.ReplyValue exposing (ReplyValue(..))
import Common.Components.Questionnaire.Fixture as Fixture
import Common.Components.Questionnaire.Nodes as Nodes exposing (ContentNode(..))
import Common.Components.Questionnaire.ViewSettings as ViewSettings
import Dict
import Expect
import Gettext
import Set exposing (Set)
import Test exposing (Test, describe, test)


itemUuid : String
itemUuid =
    "f1111111-1111-4111-8111-111111111111"


build : Set String -> List ContentNode
build collapsedPaths =
    let
        questionnaire =
            Fixture.questionnaire
                (Dict.fromList
                    [ ( Fixture.chapter1 ++ "." ++ Fixture.options, Fixture.reply (AnswerReply Fixture.optionsAnswer) )
                    , ( Fixture.chapter1 ++ "." ++ Fixture.list, Fixture.reply (ItemListReply [ itemUuid ]) )
                    ]
                )
    in
    Nodes.build
        { chapterUuid = Fixture.chapter1
        , questionnaire = questionnaire
        , collapsedPaths = collapsedPaths
        , resourcePageUrl = always "/dashboard"
        , viewSettings = ViewSettings.all
        , locale = Gettext.defaultLocale
        , knowledgeModelParentMap = KnowledgeModel.createParentMap questionnaire.knowledgeModel
        , integrationPlugins = []
        , builtInIntegrations = []
        }


kind : ContentNode -> String
kind node =
    case node of
        ChapterNode _ ->
            "chapter"

        ChapterEmptyNode ->
            "chapter-empty"

        ChapterLinksNode _ ->
            "chapter-links"

        QuestionNode data ->
            "question " ++ data.questionPath

        ItemHeaderNode _ ->
            "item-header"

        ItemEmptyNode _ ->
            "item-empty"

        ItemFooterNode _ ->
            "item-footer"

        ItemsEndNode _ ->
            "items-end"


structureTest : Test
structureTest =
    let
        c =
            Fixture.chapter1

        nodes =
            build Set.empty
    in
    describe "Nodes structure"
        [ test "is flat, in reading order: follow-ups under their answer, items under their list" <|
            \_ ->
                List.map kind nodes
                    |> Expect.equal
                        [ "chapter"
                        , "question " ++ c ++ "." ++ Fixture.options
                        , "question " ++ c ++ "." ++ Fixture.options ++ "." ++ Fixture.optionsAnswer ++ "." ++ Fixture.followUp
                        , "question " ++ c ++ "." ++ Fixture.list
                        , "item-header"
                        , "question " ++ c ++ "." ++ Fixture.list ++ "." ++ itemUuid ++ "." ++ Fixture.template
                        , "item-footer"
                        , "items-end"
                        , "question " ++ c ++ "." ++ Fixture.value1
                        , "chapter-links"
                        ]
        , test "states the nesting it draws as aria-level: a follow-up one deeper, an item one deeper, its questions two" <|
            \_ ->
                List.map Nodes.level nodes
                    |> Expect.equal
                        [ Nothing
                        , Just 1
                        , Just 2
                        , Just 1
                        , Just 2
                        , Just 3
                        , Just 2
                        , Just 2
                        , Just 1
                        , Nothing
                        ]
        , test "gives every node a key of its own" <|
            \_ ->
                let
                    keys =
                        List.map Nodes.key nodes
                in
                Set.size (Set.fromList keys)
                    |> Expect.equal (List.length keys)
        , test "leaves a collapsed item's questions out and keeps its header" <|
            \_ ->
                build (Set.singleton (c ++ "." ++ Fixture.list ++ "." ++ itemUuid))
                    |> List.map kind
                    |> Expect.equal
                        [ "chapter"
                        , "question " ++ c ++ "." ++ Fixture.options
                        , "question " ++ c ++ "." ++ Fixture.options ++ "." ++ Fixture.optionsAnswer ++ "." ++ Fixture.followUp
                        , "question " ++ c ++ "." ++ Fixture.list
                        , "item-header"
                        , "items-end"
                        , "question " ++ c ++ "." ++ Fixture.value1
                        , "chapter-links"
                        ]
        , test "leaves collapsed follow-ups out" <|
            \_ ->
                build (Set.singleton (c ++ "." ++ Fixture.options ++ "." ++ Fixture.optionsAnswer))
                    |> List.filter ((==) ("question " ++ c ++ "." ++ Fixture.options ++ "." ++ Fixture.optionsAnswer ++ "." ++ Fixture.followUp) << kind)
                    |> Expect.equal []
        , test "depth counts one per follow-up and two per item" <|
            \_ ->
                [ Nodes.ContentNesting
                , Nodes.FollowUpNesting Nodes.ContentNesting
                , Nodes.ItemNesting Nodes.ContentNesting
                , Nodes.FollowUpNesting (Nodes.ItemNesting Nodes.ContentNesting)
                , Nodes.ItemNesting (Nodes.ItemNesting Nodes.ContentNesting)
                ]
                    |> List.map Nodes.depth
                    |> Expect.equal [ 1, 2, 3, 4, 5 ]
        ]
