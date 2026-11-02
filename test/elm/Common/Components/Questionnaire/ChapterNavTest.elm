module Common.Components.Questionnaire.ChapterNavTest exposing (suite)

import Common.Api.Models.ProjectDetail.Reply.ReplyValue exposing (ReplyValue(..))
import Common.Api.Models.ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Common.Components.Questionnaire.ChapterNav as ChapterNav exposing (ChapterNav)
import Common.Components.Questionnaire.Fixture as Fixture
import Dict
import Expect
import Gettext
import Html.Attributes as Attr
import Set exposing (Set)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, class, containing, tag, text)


optionsPath : String
optionsPath =
    Fixture.chapter1 ++ "." ++ Fixture.options


answerPath : String
answerPath =
    optionsPath ++ "." ++ Fixture.optionsAnswer


listPath : String
listPath =
    Fixture.chapter1 ++ "." ++ Fixture.list


itemPath : String
itemPath =
    listPath ++ ".item-1"


build : Set String -> ProjectQuestionnaire -> ChapterNav
build collapsedPaths questionnaire =
    ChapterNav.build
        { questionnaire = questionnaire
        , chapterUuid = Fixture.chapter1
        , collapsedPaths = collapsedPaths
        , nonDesirableQuestions = True
        , kmLocale = Gettext.defaultLocale
        }


render : ChapterNav -> Query.Single ChapterNav.Msg
render nav =
    Query.fromHtml (ChapterNav.view Gettext.defaultLocale nav Dict.empty)


answered : ProjectQuestionnaire
answered =
    Fixture.questionnaire (Dict.singleton optionsPath (Fixture.reply (AnswerReply Fixture.optionsAnswer)))


withItem : Maybe String -> ProjectQuestionnaire
withItem mbName =
    Fixture.questionnaire
        (Dict.fromList
            (( listPath, Fixture.reply (ItemListReply [ "item-1" ]) )
                :: (case mbName of
                        Just name ->
                            [ ( itemPath ++ "." ++ Fixture.template, Fixture.reply (StringReply name) ) ]

                        Nothing ->
                            []
                   )
            )
        )


suite : Test
suite =
    describe "ChapterNav"
        [ test "every chapter is listed, the open one current, with its questions under it" <|
            \_ ->
                render (build Set.empty (Fixture.questionnaire Dict.empty))
                    |> Query.findAll [ class "ui-tree__entry" ]
                    |> Query.count (Expect.equal 5)
        , test "only the open chapter is the current place" <|
            \_ ->
                render (build Set.empty (Fixture.questionnaire Dict.empty))
                    |> Query.findAll [ attribute (Attr.attribute "aria-current" "true") ]
                    |> Query.count (Expect.equal 1)
        , test "a chosen answer with follow-ups is an open branch" <|
            \_ ->
                render (build Set.empty answered)
                    |> Query.find [ tag "button", attribute (Attr.attribute "aria-label" "Hide what is in Options") ]
                    |> Query.has [ attribute (Attr.attribute "aria-expanded" "true") ]
        , test "the follow-up is listed under the answer" <|
            \_ ->
                render (build Set.empty answered)
                    |> Query.has [ text "Follow-up" ]
        , test "a collapsed answer folds its follow-ups away" <|
            \_ ->
                render (build (Set.singleton answerPath) answered)
                    |> Query.hasNot [ text "Follow-up" ]
        , test "an item is named by its first question" <|
            \_ ->
                render (build Set.empty (withItem (Just "Alpha")))
                    |> Query.has [ tag "button", containing [ text "Alpha" ] ]
        , test "an unnamed item is Item and its number, as a stand-in" <|
            \_ ->
                render (build Set.empty (withItem Nothing))
                    |> Query.find [ class "ui-tree__entry", class "is-placeholder" ]
                    |> Query.has [ text "Item 1" ]
        , test "refreshing item titles that did not change keeps the navigation" <|
            \_ ->
                let
                    nav =
                        build Set.empty (withItem (Just "Alpha"))
                in
                ChapterNav.refreshItemTitles (withItem (Just "Alpha")) nav
                    |> Expect.equal nav
        , test "refreshing takes a renamed item's new title" <|
            \_ ->
                build Set.empty (withItem (Just "Alpha"))
                    |> ChapterNav.refreshItemTitles (withItem (Just "Beta"))
                    |> Expect.equal (build Set.empty (withItem (Just "Beta")))
        ]
