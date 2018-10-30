module Common.View.FormsTest exposing (..)

import ActionResult exposing (ActionResult(..))
import Common.View.Forms exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (string)
import Html.Attributes as Attr
import Msgs exposing (Msg(..))
import Routing exposing (Route(..))
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, classes, tag, text)
import TestUtils exposing (parametrized)


formActionsTests : Test
formActionsTests =
    describe "formActions"
        [ test "should contain cancel button" <|
            \_ ->
                formActions Welcome ( "Action", Unset, ChangeLocation "/welcome" )
                    |> Query.fromHtml
                    |> Query.find [ tag "a" ]
                    |> Query.has [ attribute <| Attr.href "/welcome" ]
        , test "should contain an action button" <|
            \_ ->
                formActions Welcome ( "Action", Unset, ChangeLocation "/" )
                    |> Query.fromHtml
                    |> Query.find [ tag "button" ]
                    |> Query.has [ text "Action" ]
        ]


formActionOnlyTests : Test
formActionOnlyTests =
    describe "formActionOnly"
        [ test "should contain an action button" <|
            \_ ->
                formActionOnly ( "Action", Unset, ChangeLocation "/" )
                    |> Query.fromHtml
                    |> Query.find [ tag "button" ]
                    |> Query.has [ text "Action" ]
        ]


actionButtonTests : Test
actionButtonTests =
    describe "actionButton"
        [ fuzz string "should have correct action name" <|
            \str ->
                actionButton ( str, Unset, ChangeLocation "/" )
                    |> Query.fromHtml
                    |> Query.has [ text str ]
        , test "should be disabled while loading" <|
            \_ ->
                actionButton ( "Action", Loading, ChangeLocation "/" )
                    |> Query.fromHtml
                    |> Query.has [ attribute <| Attr.disabled True ]
        , parametrized [ Unset, Success "", Error "" ] "should not be disabled when not loading" <|
            \state ->
                actionButton ( "Action", state, ChangeLocation "/" )
                    |> Query.fromHtml
                    |> Query.has [ attribute <| Attr.disabled False ]
        , test "should contain a loader while loading" <|
            \_ ->
                actionButton ( "Action", Loading, ChangeLocation "/" )
                    |> Query.fromHtml
                    |> Query.find [ tag "i" ]
                    |> Query.has [ classes [ "fa", "fa-spinner", "fa-spin" ] ]
        ]
