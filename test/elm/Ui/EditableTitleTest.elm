module Ui.EditableTitleTest exposing (suite)

import Expect
import Html.Attributes as Attr
import Json.Encode as Encode
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, id, tag, text)
import Ui.AppShell as AppShell
import Ui.EditableTitle as EditableTitle exposing (Outcome(..))


{-| What a rename asks for, what puts the old name back, and the keys — the
part a page builds its saving on.
-}
suite : Test
suite =
    describe "Ui.EditableTitle"
        [ describe "update"
            [ test "Enter asks for the typed name, trimmed" <|
                \_ ->
                    run [ EditableTitle.start name, EditableTitle.input "  Grant proposal ", EditableTitle.commit ]
                        |> Expect.equal ( False, Renamed "Grant proposal" )
            , test "clicking away asks for the typed name too" <|
                \_ ->
                    run [ EditableTitle.start name, EditableTitle.input "Grant proposal", EditableTitle.blur ]
                        |> Expect.equal ( False, Renamed "Grant proposal" )
            , test "Escape keeps the name" <|
                \_ ->
                    run [ EditableTitle.start name, EditableTitle.input "Grant proposal", EditableTitle.cancel ]
                        |> Expect.equal ( False, Unchanged )
            , test "an empty name is not asked for" <|
                \_ ->
                    run [ EditableTitle.start name, EditableTitle.input "   ", EditableTitle.commit ]
                        |> Expect.equal ( False, Unchanged )
            , test "the same name is not asked for" <|
                \_ ->
                    run [ EditableTitle.start name, EditableTitle.input (" " ++ name), EditableTitle.commit ]
                        |> Expect.equal ( False, Unchanged )
            , test "the blur that follows Enter asks for nothing more" <|
                \_ ->
                    run [ EditableTitle.start name, EditableTitle.input "Grant proposal", EditableTitle.commit, EditableTitle.blur ]
                        |> Expect.equal ( False, Unchanged )
            , test "typing keeps it editing" <|
                \_ ->
                    run [ EditableTitle.start name, EditableTitle.input "Grant" ]
                        |> Expect.equal ( True, Unchanged )
            ]
        , describe "view"
            [ test "the name is a button inside the page's h1" <|
                \_ ->
                    view EditableTitle.init False
                        |> Query.has [ tag "h1", id AppShell.mainHeadingId ]
            , test "pressing the name starts editing it" <|
                \_ ->
                    view EditableTitle.init False
                        |> Query.find [ tag "button" ]
                        |> Event.simulate Event.click
                        |> Event.toResult
                        |> Result.map (\msg -> Tuple.first (run [ msg ]))
                        |> Expect.equal (Ok True)
            , test "while saving, the name cannot be pressed" <|
                \_ ->
                    view EditableTitle.init True
                        |> Query.find [ tag "button", attribute (Attr.attribute "aria-disabled" "true") ]
                        |> Event.simulate Event.click
                        |> Event.toResult
                        |> Expect.err
            , test "the field is named by its label" <|
                \_ ->
                    view (editing "Grant") False
                        |> Query.find [ tag "input" ]
                        |> Query.has [ attribute (Attr.attribute "aria-label" "Project name") ]
            , test "Enter in the field commits" <|
                \_ ->
                    view (editing "Grant proposal") False
                        |> Query.find [ tag "input" ]
                        |> Event.simulate (keydown "Enter" False)
                        |> Event.toResult
                        |> Result.map (\msg -> runFrom (editing "Grant proposal") [ msg ])
                        |> Expect.equal (Ok ( False, Renamed "Grant proposal" ))
            , test "Enter while an input method is composing does nothing" <|
                \_ ->
                    view (editing "生物") False
                        |> Query.find [ tag "input" ]
                        |> Event.simulate (keydown "Enter" True)
                        |> Event.toResult
                        |> Expect.err
            , test "Escape in the field cancels" <|
                \_ ->
                    view (editing "Grant proposal") False
                        |> Query.find [ tag "input" ]
                        |> Event.simulate (keydown "Escape" False)
                        |> Event.toResult
                        |> Result.map (\msg -> runFrom (editing "Grant proposal") [ msg ])
                        |> Expect.equal (Ok ( False, Unchanged ))
            , test "the name carries the page's test id" <|
                \_ ->
                    view EditableTitle.init False
                        |> Query.find [ tag "button" ]
                        |> Query.has [ attribute (Attr.attribute "data-cy" "project_detail_title") ]
            , test "the field carries it with -input after it" <|
                \_ ->
                    view (editing "Grant") False
                        |> Query.find [ tag "input" ]
                        |> Query.has [ attribute (Attr.attribute "data-cy" "project_detail_title-input") ]
            , test "the heading reads as the name" <|
                \_ ->
                    view EditableTitle.init False
                        |> Query.find [ tag "button" ]
                        |> Query.has [ text name ]
            ]
        ]


name : String
name =
    "Data management plan"


{-| Whether it is still editing after the messages, and what the last one asked
for.
-}
run : List EditableTitle.Msg -> ( Bool, Outcome )
run =
    runFrom EditableTitle.init


runFrom : EditableTitle.Model -> List EditableTitle.Msg -> ( Bool, Outcome )
runFrom model msgs =
    List.foldl
        (\msg ( m, _ ) ->
            let
                ( next, _, outcome ) =
                    EditableTitle.update name msg m
            in
            ( next, outcome )
        )
        ( model, Unchanged )
        msgs
        |> Tuple.mapFirst EditableTitle.isEditing


editing : String -> EditableTitle.Model
editing typed =
    [ EditableTitle.start name, EditableTitle.input typed ]
        |> List.foldl (\msg m -> (\( next, _, _ ) -> next) (EditableTitle.update name msg m)) EditableTitle.init


view : EditableTitle.Model -> Bool -> Query.Single EditableTitle.Msg
view model saving =
    EditableTitle.view
        { value = name
        , model = model
        , toMsg = identity
        , label = "Project name"
        , hint = "Rename"
        , saving = saving
        , testId = "project_detail_title"
        }
        |> Query.fromHtml


keydown : String -> Bool -> ( String, Encode.Value )
keydown key composing =
    Event.custom "keydown"
        (Encode.object
            [ ( "key", Encode.string key )
            , ( "isComposing", Encode.bool composing )
            , ( "keyCode", Encode.int 0 )
            ]
        )
