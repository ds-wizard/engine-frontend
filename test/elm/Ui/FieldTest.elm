module Ui.FieldTest exposing (suite)

import Expect
import Html
import Html.Attributes as Attr
import Json.Encode as Encode
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, class, tag)
import Ui.Field as Field


{-| The password meter says its strength in words.

Which is the whole point of the test: the existing meter is five colored bars
and nothing else, so it says nothing to a screen reader and nothing to anyone who
cannot separate red from green. The wording itself is the library's; what must
hold is that there _is_ wording, that an empty field says nothing rather than
"very weak", and that the words change as the password gets better.

-}
suite : Test
suite =
    describe "Ui.Field"
        [ describe "the password strength meter"
            [ test "says nothing about an empty field" <|
                \_ ->
                    Field.strengthLabel ""
                        |> Expect.equal ""
            , test "says something about a bad password" <|
                \_ ->
                    Field.strengthLabel "a"
                        |> Expect.notEqual ""
            , test "says something else about a good one" <|
                \_ ->
                    Field.strengthLabel "7Qw!pLz2vR#hT4xM"
                        |> Expect.notEqual (Field.strengthLabel "a")
            , test "does not change its mind between calls" <|
                \_ ->
                    Field.strengthLabel "correct horse battery staple"
                        |> Expect.equal (Field.strengthLabel "correct horse battery staple")
            ]
        , describe "the color picker follows the typed value" <|
            List.map
                (\( typed, shown ) ->
                    test (typed ++ " shows as " ++ shown) <|
                        \_ ->
                            colorField typed
                                |> Query.find [ tag "input", attribute (Attr.type_ "color") ]
                                |> Query.has [ attribute (Attr.value shown) ]
                )
                [ ( "#ff0000", "#ff0000" )
                , ( "#FF0000", "#ff0000" )
                , ( "#fff", "#ffffff" )
                , ( "FFF", "#ffffff" )
                , ( " #abc ", "#aabbcc" )
                , ( "#11223344", "#112233" )
                , ( "#1234", "#112233" )
                , ( "#ff000", "#000000" )
                , ( "#ggg", "#000000" )
                , ( "", "#000000" )
                ]
        , describe "a control on a choice group's name's line"
            [ test "is drawn beside the name" <|
                \_ ->
                    tagsField True
                        |> Query.find [ class "ui-field__label-row" ]
                        |> Query.has [ tag "legend", tag "button" ]
            , test "and the group is still named by it" <|
                \_ ->
                    -- A `legend` names its `fieldset` only as the fieldset's
                    -- first child, which it no longer is.
                    tagsField True
                        |> Query.has [ tag "fieldset", attribute (Attr.attribute "aria-labelledby" "tags-label") ]
            , test "and the name is left where it was without one" <|
                \_ ->
                    tagsField False
                        |> Query.findAll [ class "ui-field__label-row" ]
                        |> Query.count (Expect.equal 0)
            ]
        , describe "a box that sends on Enter" <|
            List.map
                (\( name, key, sends ) ->
                    test name <|
                        \_ ->
                            messageBox Field.withSubmitOnEnter
                                |> Event.simulate (Event.custom "keydown" key)
                                |> (if sends then
                                        Event.expect "sent"

                                    else
                                        Event.toResult >> Expect.err
                                   )
                )
                [ ( "sends on Enter", enter { shift = False, ctrl = False, composing = False }, True )
                , ( "makes a new line on Shift+Enter", enter { shift = True, ctrl = False, composing = False }, False )
                , ( "sends on Ctrl+Enter", enter { shift = False, ctrl = True, composing = False }, True )
                , ( "leaves Enter to an input method composing a word", enter { shift = False, ctrl = False, composing = True }, False )
                ]
        , describe "a box with the submit shortcut"
            [ test "makes a new line on Enter" <|
                \_ ->
                    messageBox Field.withSubmitShortcut
                        |> Event.simulate (Event.custom "keydown" (enter { shift = False, ctrl = False, composing = False }))
                        |> Event.toResult
                        |> Expect.err
            , test "sends on Ctrl+Enter" <|
                \_ ->
                    messageBox Field.withSubmitShortcut
                        |> Event.simulate (Event.custom "keydown" (enter { shift = False, ctrl = True, composing = False }))
                        |> Event.expect "sent"
            ]
        ]


messageBox : (String -> Field.Field String -> Field.Field String) -> Query.Single String
messageBox withSubmit =
    Field.textarea { id = "message", label = "Message", value = "", onInput = identity }
        |> withSubmit "sent"
        |> Field.toHtml
        |> Query.fromHtml
        |> Query.find [ tag "textarea" ]


enter : { shift : Bool, ctrl : Bool, composing : Bool } -> Encode.Value
enter modifiers =
    Encode.object
        [ ( "key", Encode.string "Enter" )
        , ( "metaKey", Encode.bool False )
        , ( "ctrlKey", Encode.bool modifiers.ctrl )
        , ( "shiftKey", Encode.bool modifiers.shift )
        , ( "isComposing", Encode.bool modifiers.composing )
        ]


{-| The checkbox group the knowledge model editor's preview narrows itself with:
a tag to tick, and a button acting on the whole set beside its name.
-}
tagsField : Bool -> Query.Single ()
tagsField withAction =
    Field.checkboxGroup
        { id = "tags"
        , label = "Question tags"
        , values = []
        , options = [ Field.choice { value = "core", label = "Core" } ]
        , onToggle = always ()
        }
        |> (if withAction then
                Field.withLabelAction (Html.button [] [ Html.text "Select all" ])

            else
                identity
           )
        |> Field.toHtml
        |> Query.fromHtml


colorField : String -> Query.Single ()
colorField value =
    Field.color { id = "color", label = "Color", value = value, onInput = always () }
        |> Field.toHtml
        |> Query.fromHtml
