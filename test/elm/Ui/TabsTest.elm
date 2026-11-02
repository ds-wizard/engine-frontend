module Ui.TabsTest exposing (suite)

import Expect
import Html exposing (Html)
import Html.Attributes as Attr
import Json.Encode as Encode
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, id, tag)
import Ui.Tabs as Tabs


type Source
    = File
    | Registry
    | Owl


type Msg
    = Selected Source


selected : Msg -> Source
selected (Selected source) =
    source


{-| The half of the keyboard fix that can be checked without a browser: every
tab button has the id `focusTab` looks for, and an arrow key selects the tab the
focus is meant to move to. The move itself is `Browser.Dom.focus`, which elm-test
cannot run.
-}
suite : Test
suite =
    describe "Ui.Tabs"
        [ test "toHtml numbers the tab buttons" <|
            \_ ->
                tabs (Tabs.toHtml (config Registry))
                    |> Query.find [ id (Tabs.tabElementId "source" "2") ]
                    |> Query.has [ tag "button", attribute (Attr.attribute "role" "tab") ]
        , test "toHtmlWithTabIds names the tab buttons by their own ids" <|
            \_ ->
                tabs (Tabs.toHtmlWithTabIds sourceKey (config Registry))
                    |> Query.find [ id (Tabs.tabElementId "source" "registry") ]
                    |> Query.has [ attribute (Attr.attribute "aria-selected" "true") ]
        , test "the element id is the tablist's id, -tab-, and the key" <|
            \_ ->
                Tabs.tabElementId "source" "owl"
                    |> Expect.equal "source-tab-owl"
        , test "ArrowRight selects the next tab" <|
            \_ ->
                arrow "ArrowRight" Registry
                    |> Expect.equal (Ok Owl)
        , test "ArrowRight wraps from the last tab to the first" <|
            \_ ->
                arrow "ArrowRight" Owl
                    |> Expect.equal (Ok File)
        , test "ArrowLeft wraps from the first tab to the last" <|
            \_ ->
                arrow "ArrowLeft" File
                    |> Expect.equal (Ok Owl)
        , test "Home and End go to the ends" <|
            \_ ->
                ( arrow "Home" Owl, arrow "End" File )
                    |> Expect.equal ( Ok File, Ok Owl )
        ]


config : Source -> { id : String, label : String, active : Source, tabs : List (Tabs.Tab Source Msg), full : Bool }
config active =
    { id = "source"
    , label = "Import from"
    , active = active
    , tabs =
        List.map
            (\source -> Tabs.tab { id = source, label = sourceKey source, onSelect = Selected source })
            [ File, Registry, Owl ]
    , full = False
    }


sourceKey : Source -> String
sourceKey source =
    case source of
        File ->
            "file"

        Registry ->
            "registry"

        Owl ->
            "owl"


tabs : Html Msg -> Query.Single Msg
tabs =
    Query.fromHtml


arrow : String -> Source -> Result String Source
arrow key active =
    tabs (Tabs.toHtml (config active))
        |> Query.find [ attribute (Attr.attribute "role" "tablist") ]
        |> Event.simulate (Event.custom "keydown" (Encode.object [ ( "key", Encode.string key ) ]))
        |> Event.toResult
        |> Result.map selected
