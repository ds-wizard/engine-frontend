module Ui.ComboboxTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Ui.Combobox as Combobox


type alias Item =
    { name : String }


{-| State transitions only.

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
        ]


noItem : Maybe Item
noItem =
    Nothing


update : Combobox.Msg Item -> Combobox.Model Item -> Combobox.Model Item
update msg model =
    Tuple.first (Combobox.update "test" 4 msg model)
