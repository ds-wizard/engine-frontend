module Ui.TreeTest exposing (suite)

import Expect
import Html
import Html.Attributes as Attr
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, class, tag, text)
import Ui.Tree as Tree


type Msg
    = SelectedAlpha
    | SelectedInner
    | SelectedBeta
    | ToggledAlpha


words : Tree.Words
words =
    { expand = \label -> "Show " ++ label, collapse = \label -> "Hide " ++ label }


tree : Bool -> Query.Single Msg
tree expanded =
    Query.fromHtml
        (Tree.toHtml { label = "Chapters", words = words }
            [ Tree.node { key = "a", label = "Alpha", onSelect = SelectedAlpha }
                |> Tree.withCurrent True
                |> Tree.withChildren { expanded = expanded, onToggle = ToggledAlpha }
                    [ Tree.node { key = "a1", label = "Inner", onSelect = SelectedInner } ]
            , Tree.node { key = "b", label = "Beta", onSelect = SelectedBeta }
            ]
        )


{-| The contract a navigation tree makes to assistive technology: nested lists, a
disclosure button per branch that says whether it is open, and the current place.
-}
suite : Test
suite =
    describe "Ui.Tree"
        [ test "the tree is a named list" <|
            \_ ->
                tree True
                    |> Query.has [ tag "ul", attribute (Attr.attribute "aria-label" "Chapters") ]
        , test "an open branch's toggle is expanded and named for hiding" <|
            \_ ->
                tree True
                    |> Query.find [ class "ui-tree__toggle" ]
                    |> Query.has [ attribute (Attr.attribute "aria-expanded" "true"), attribute (Attr.attribute "aria-label" "Hide Alpha") ]
        , test "a folded branch's toggle is collapsed and named for showing" <|
            \_ ->
                tree False
                    |> Query.find [ class "ui-tree__toggle" ]
                    |> Query.has [ attribute (Attr.attribute "aria-expanded" "false"), attribute (Attr.attribute "aria-label" "Show Alpha") ]
        , test "a folded branch renders no children" <|
            \_ ->
                tree False
                    |> Query.hasNot [ text "Inner" ]
        , test "an open branch's children are a nested list" <|
            \_ ->
                tree True
                    |> Query.find [ class "ui-tree__group" ]
                    |> Query.has [ text "Inner" ]
        , test "the current place carries aria-current" <|
            \_ ->
                tree True
                    |> Query.findAll [ attribute (Attr.attribute "aria-current" "true") ]
                    |> Query.count (Expect.equal 1)
        , test "choosing an entry sends its message" <|
            \_ ->
                tree True
                    |> Query.findAll [ class "ui-tree__entry" ]
                    |> Query.index 2
                    |> Event.simulate Event.click
                    |> Event.expect SelectedBeta
        , test "the toggle sends the branch's message" <|
            \_ ->
                tree True
                    |> Query.find [ class "ui-tree__toggle" ]
                    |> Event.simulate Event.click
                    |> Event.expect ToggledAlpha
        , test "what happened to an entry is a class as well as the word at its end" <|
            \_ ->
                marked
                    |> Query.find [ class "is-danger" ]
                    |> Query.has [ text "Deleted" ]
        , test "an entry with nothing behind it is out of reach" <|
            \_ ->
                marked
                    |> Query.find [ class "is-danger" ]
                    |> Query.has [ attribute (Attr.disabled True) ]
        , test "an entry that makes something is marked as one" <|
            \_ ->
                marked
                    |> Query.find [ class "is-action" ]
                    |> Query.has [ text "Add chapter" ]
        , test "the entry the tree is about is picked out" <|
            \_ ->
                marked
                    |> Query.find [ class "is-highlighted" ]
                    |> Query.has [ text "Alpha" ]
        , test "the thing everything else is inside takes the whole width" <|
            \_ ->
                marked
                    |> Query.find [ class "is-root" ]
                    |> Query.has [ text "Alpha" ]
        , test "an entry that makes something is still a button that can be pressed" <|
            \_ ->
                marked
                    |> Query.find [ class "is-action" ]
                    |> Event.simulate Event.click
                    |> Event.expect SelectedBeta
        ]


{-| The three things a tree says about an entry beyond where it goes: what has
happened to it, whether there is anything behind it, and whether it makes
something instead.
-}
marked : Query.Single Msg
marked =
    Query.fromHtml
        (Tree.toHtml { label = "Chapters", words = words }
            [ Tree.node { key = "a", label = "Alpha", onSelect = SelectedAlpha }
                |> Tree.withTone Tree.Danger
                |> Tree.withDisabled True
                |> Tree.withRoot
                |> Tree.withHighlight
                |> Tree.withEnd [ Html.text "Deleted" ]
            , Tree.node { key = "add", label = "Add chapter", onSelect = SelectedBeta }
                |> Tree.withAction
            ]
        )
