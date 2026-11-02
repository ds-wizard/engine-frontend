module Ui.IconTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Ui.Icon as Icon


{-| An icon named by data keeps its weight, and nothing but Font Awesome's own
classes reaches the element.
-}
suite : Test
suite =
    let
        classes =
            Icon.className << Icon.fromClass
    in
    describe "Ui.Icon.fromClass"
        [ test "keeps regular" <|
            \_ -> classes "far fa-file-pdf" |> Expect.equal "far fa-file-pdf"
        , test "keeps solid" <|
            \_ -> classes "fas fa-code" |> Expect.equal "fas fa-code"
        , test "keeps brands" <|
            \_ -> classes "fab fa-github" |> Expect.equal "fab fa-github"
        , test "understands the long weight names" <|
            \_ -> classes "fa-brands fa-github" |> Expect.equal "fab fa-github"
        , test "is solid when no weight is given" <|
            \_ -> classes "fa-wand-magic" |> Expect.equal "fas fa-wand-magic"
        , test "the old fa prefix is solid" <|
            \_ -> classes "fa fa-wand-magic" |> Expect.equal "fas fa-wand-magic"
        , test "tolerates extra whitespace and the weight after the glyph" <|
            \_ -> classes "  fa-github   fab " |> Expect.equal "fab fa-github"
        , test "keeps modifiers" <|
            \_ -> classes "fas fa-spinner fa-spin" |> Expect.equal "fas fa-spinner fa-spin"
        , test "drops anything that is not Font Awesome" <|
            \_ -> classes "far fa-file ui-button evil" |> Expect.equal "far fa-file"
        , test "a string with no icon is a question mark" <|
            \_ -> classes "not an icon" |> Expect.equal "fas fa-question"
        , test "an empty string is a question mark" <|
            \_ -> classes "" |> Expect.equal "fas fa-question"
        ]
