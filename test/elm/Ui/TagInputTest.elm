module Ui.TagInputTest exposing (suite)

import Expect
import Html.Attributes as Attr
import Json.Encode as Encode
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, id, tag)
import Ui.TagInput as TagInput


type Msg
    = Typed
    | Added String
    | Removed String


{-| The tag a message carries, so a test can compare what was added rather than
only that something was.
-}
tagOf : Msg -> Maybe String
tagOf msg =
    case msg of
        Added value ->
            Just value

        Removed value ->
            Just value

        Typed ->
            Nothing


{-| Enter and the add button, and the one case that is easy to get wrong
without noticing: an Enter that belongs to an input method.
-}
suite : Test
suite =
    describe "Ui.TagInput"
        [ test "Enter adds the typed text, trimmed" <|
            \_ ->
                field "  biology "
                    |> Query.find [ id "tags" ]
                    |> Event.simulate (keydown { key = "Enter", isComposing = False, keyCode = 13 })
                    |> Event.toResult
                    |> Result.map tagOf
                    |> Expect.equal (Ok (Just "biology"))
        , test "Enter while an input method is composing does not add" <|
            \_ ->
                field "生物"
                    |> Query.find [ id "tags" ]
                    |> Event.simulate (keydown { key = "Enter", isComposing = True, keyCode = 13 })
                    |> Event.toResult
                    |> Expect.err
        , test "the Enter that ends a composition in Safari does not add" <|
            \_ ->
                field "生物"
                    |> Query.find [ id "tags" ]
                    |> Event.simulate (keydown { key = "Enter", isComposing = False, keyCode = 229 })
                    |> Event.toResult
                    |> Expect.err
        , test "Enter with nothing typed does not add" <|
            \_ ->
                field "   "
                    |> Query.find [ id "tags" ]
                    |> Event.simulate (keydown { key = "Enter", isComposing = False, keyCode = 13 })
                    |> Event.toResult
                    |> Expect.err
        , test "the add button is disabled with nothing typed" <|
            \_ ->
                field ""
                    |> Query.find [ tag "button", attribute (Attr.disabled True) ]
                    |> Query.has [ tag "button" ]
        , test "each remove button is named after its tag" <|
            \_ ->
                field ""
                    |> Query.find [ attribute (Attr.attribute "aria-label" "Remove tag open data") ]
                    |> Event.simulate Event.click
                    |> Event.expect (Removed "open data")
        ]


field : String -> Query.Single Msg
field typed =
    TagInput.tagInput
        { id = "tags"
        , label = "Project tags"
        , values = [ "biology", "open data" ]
        , input = typed
        , onInput = always Typed
        , onAdd = Added
        , onRemove = Removed
        , addLabel = "Add tag"
        , removeLabel = \value -> "Remove tag " ++ value
        }
        |> TagInput.toHtml
        |> Query.fromHtml


keydown : { key : String, isComposing : Bool, keyCode : Int } -> ( String, Encode.Value )
keydown event =
    Event.custom "keydown"
        (Encode.object
            [ ( "key", Encode.string event.key )
            , ( "isComposing", Encode.bool event.isComposing )
            , ( "keyCode", Encode.int event.keyCode )
            ]
        )
