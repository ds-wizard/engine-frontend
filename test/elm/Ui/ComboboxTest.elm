module Ui.ComboboxTest exposing (suite)

import Expect exposing (Expectation)
import Html
import Html.Attributes
import Json.Encode as Encode
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, class, tag)
import Ui.Combobox as Combobox


type alias Item =
    { name : String }


{-| State transitions, and the keys of the search field.

Note what these do **not** cover. The trigger opens on focus and choosing an
item hands focus back to it, so anything that closes by returning focus must
also suppress the reopen — a guard that existed for Escape and not for selection
until it was reported. That reopen happens through a DOM focus event, which
elm-test cannot fire: these tests pass with the bug reintroduced.

What actually prevents it is structural rather than tested — `closeReturningFocus`
makes closing and returning focus one operation, so no path can do one without
the other. These tests cover the rest of the machine, which is worth having, but
the reopen needs a browser.

-}
suite : Test
suite =
    describe "Ui.Combobox"
        [ test "starts closed with nothing chosen" <|
            \_ ->
                Combobox.init
                    |> Expect.all
                        [ Combobox.isOpen >> Expect.equal False
                        , Combobox.selected >> Expect.equal noItem
                        ]
        , test "choosing an item closes the popup" <|
            \_ ->
                Combobox.init
                    |> update (Combobox.select { name = "Data management plan" })
                    |> Combobox.isOpen
                    |> Expect.equal False
        , test "choosing an item records the choice" <|
            \_ ->
                Combobox.init
                    |> update (Combobox.select { name = "Data management plan" })
                    |> Combobox.selected
                    |> Expect.equal (Just { name = "Data management plan" })
        , test "choosing an item clears the search query" <|
            \_ ->
                Combobox.init
                    |> update (Combobox.select { name = "Life sciences KM" })
                    |> Combobox.query
                    |> Expect.equal ""
        , test "clearing removes the choice and stays closed" <|
            \_ ->
                Combobox.init
                    |> update (Combobox.select { name = "Life sciences KM" })
                    |> Combobox.clear
                    |> Expect.all
                        [ Combobox.isOpen >> Expect.equal False
                        , Combobox.selected >> Expect.equal noItem
                        ]
        , test "a closed popup asks for no results" <|
            \_ ->
                Combobox.resultsWanted { before = Combobox.init, after = Combobox.init }
                    |> Expect.equal Nothing
        , test "choosing an item asks for no results" <|
            \_ ->
                Combobox.resultsWanted
                    { before = Combobox.init
                    , after = update (Combobox.select { name = "Life sciences KM" }) Combobox.init
                    }
                    |> Expect.equal Nothing
        , describe "Enter in the search field"
            -- The search field is a text field inside the form around the
            -- combobox: an Enter it lets through submits that form.
            [ test "is kept from the form while the options are loading" <|
                \_ -> expectEnterKept (\c -> { c | loading = True })
            , test "is kept from the form when nothing matches" <|
                \_ -> expectEnterKept identity
            , test "chooses the highlighted option" <|
                \_ ->
                    opened (config [ { name = "Data management plan" } ])
                        |> pressEnter (config [ { name = "Data management plan" } ])
                        |> Event.toResult
                        |> Result.map (\msg -> update msg Combobox.init |> Combobox.selected)
                        |> Expect.equal (Ok (Just { name = "Data management plan" }))
            ]
        ]


expectEnterKept : (Combobox.Config Item -> Combobox.Config Item) -> Expectation
expectEnterKept change =
    let
        cfg =
            change (config [])
    in
    opened cfg
        |> pressEnter cfg
        |> Event.expectPreventDefault


{-| Opened the way a person opens it: a click on the trigger.
-}
opened : Combobox.Config Item -> Combobox.Model Item
opened cfg =
    Combobox.toHtml cfg Combobox.init
        |> Query.fromHtml
        |> Query.find [ class "ui-combobox__trigger" ]
        |> Event.simulate Event.click
        |> Event.toResult
        |> Result.map (\msg -> update msg Combobox.init)
        |> Result.withDefault Combobox.init


pressEnter : Combobox.Config Item -> Combobox.Model Item -> Event.Event (Combobox.Msg Item)
pressEnter cfg model =
    Combobox.toHtml cfg model
        |> Query.fromHtml
        |> Query.find [ tag "input", attribute (Html.Attributes.attribute "role" "combobox") ]
        |> Event.simulate (Event.custom "keydown" (Encode.object [ ( "key", Encode.string "Enter" ) ]))


config : List Item -> Combobox.Config Item
config options =
    { id = "test"
    , label = "Document template"
    , placeholder = "Select document template"
    , searchPlaceholder = "Search"
    , options = options
    , viewOption = \item -> Html.text item.name
    , optionTestId = .name
    , viewSelected = \item -> Html.text item.name
    , loading = False
    , loadingLabel = "Loading"
    , emptyLabel = "Nothing matches"
    , resultsLabel = String.fromInt
    , clearLabel = Nothing
    }


noItem : Maybe Item
noItem =
    Nothing


update : Combobox.Msg Item -> Combobox.Model Item -> Combobox.Model Item
update msg model =
    Tuple.first (Combobox.update "test" 4 msg model)
