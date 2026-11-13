module Ui.EditableTitle exposing
    ( Config
    , Model
    , Msg
    , Outcome(..)
    , blur
    , cancel
    , commit
    , init
    , input
    , isEditing
    , start
    , update
    , view
    )

{-| A page title that can be renamed where it stands. Press the name, type, and
press Enter or click away; Escape keeps the name it had.

**The `h1` stays the `h1`.** The name inside it is a button styled as the text
it is, so the heading still reads as the name, focus after a route change still
lands on it (`Ui.AppShell.mainHeadingId`), and pressing it is in the tab order.
While it is being edited the button gives way to an `input` in the same place
and at the same size; there is no `contenteditable`, whose caret, paste and
undo would all be ours to get right.

**The page saves; this only says what was asked for.** `update` hands back an
`Outcome`: `Renamed` with the new name, trimmed, when it is not empty and not
what the title already says, and `Unchanged` otherwise. An empty name is
never asked for — it puts the old one back, as Escape does. What the page
shows while the request is away, and what it puts back when it fails, is the
page's.

**Focus goes back to the name after Enter and Escape**, which took it from
there; not after clicking away, which already put it somewhere else.

**Enter does nothing while a word is being composed**, as in `Ui.TagInput`.

The text is selected when editing starts (`js/editable-title.js`), so typing
replaces the name and an arrow key keeps it.

-}

import Browser.Dom as Dom
import Html exposing (Html, button, h1, text)
import Html.Attributes as Attr exposing (attribute, class, id, title, type_, value)
import Html.Events exposing (onBlur, onClick, onInput, preventDefaultOn)
import Json.Decode as Decode
import Task
import Ui.AppShell as AppShell
import Ui.TestId as TestId


{-| Whether the name is being edited, and what has been typed if it is.
-}
type Model
    = Model (Maybe String)


init : Model
init =
    Model Nothing


isEditing : Model -> Bool
isEditing (Model draft) =
    draft /= Nothing


type Msg
    = Start String
    | Input String
    | Commit
    | Blur
    | Cancel
    | Focused


{-| Start editing a title that says `name` — what pressing the name does.
-}
start : String -> Msg
start =
    Start


{-| What typing does.
-}
input : String -> Msg
input =
    Input


{-| What Enter does.
-}
commit : Msg
commit =
    Commit


{-| What leaving the field does.
-}
blur : Msg
blur =
    Blur


{-| What Escape does.
-}
cancel : Msg
cancel =
    Cancel


{-| What the user asked for: a new name, or none.
-}
type Outcome
    = Renamed String
    | Unchanged


{-| `name` is what the title says now, against which the typed name is compared.
-}
update : String -> Msg -> Model -> ( Model, Cmd Msg, Outcome )
update name msg (Model draft) =
    case ( msg, draft ) of
        ( Start current, Nothing ) ->
            ( Model (Just current), focus inputId, Unchanged )

        ( Input typed, Just _ ) ->
            ( Model (Just typed), Cmd.none, Unchanged )

        ( Commit, Just typed ) ->
            ( Model Nothing, focus buttonId, outcomeOf name typed )

        ( Blur, Just typed ) ->
            ( Model Nothing, Cmd.none, outcomeOf name typed )

        ( Cancel, Just _ ) ->
            ( Model Nothing, focus buttonId, Unchanged )

        -- Enter takes the field away, which can blur it too: the blur that
        -- follows finds nothing being edited and is ignored.
        _ ->
            ( Model draft, Cmd.none, Unchanged )


outcomeOf : String -> String -> Outcome
outcomeOf name typed =
    let
        trimmed =
            String.trim typed
    in
    if String.isEmpty trimmed || trimmed == name then
        Unchanged

    else
        Renamed trimmed


focus : String -> Cmd Msg
focus elementId =
    Task.attempt (always Focused) (Dom.focus elementId)


{-| `label` names the field while it is edited — "Project name". `hint` says
what pressing the name does — "Rename" — as its tooltip, which is also its
description to a screen reader: the name is the button's own words, so the
heading still reads as the name.

While `saving`, the name cannot be pressed: what it says is on its way to the
server.

`testId` is the name's hook for end-to-end tests, as the page names it —
`project_detail_title`; the field it gives way to while edited is the same
with `-input` after it. Both are one per page, as the title is.

-}
type alias Config msg =
    { value : String
    , model : Model
    , toMsg : Msg -> msg
    , label : String
    , hint : String
    , saving : Bool
    , testId : String
    }


view : Config msg -> Html msg
view config =
    let
        (Model draft) =
            config.model
    in
    h1 [ class "ui-page-title ui-editable-title", id AppShell.mainHeadingId, Attr.tabindex -1 ]
        [ case draft of
            Just typed ->
                Html.input
                    [ class "ui-editable-title__input"
                    , id inputId
                    , type_ "text"
                    , value typed
                    , attribute "aria-label" config.label
                    , attribute "data-ui-editable-title" ""
                    , TestId.attribute (config.testId ++ "-input")
                    , onInput (config.toMsg << Input)
                    , onBlur (config.toMsg Blur)
                    , preventDefaultOn "keydown" (Decode.map (\m -> ( config.toMsg m, True )) keyDecoder)
                    ]
                    []

            Nothing ->
                button
                    ([ class "ui-editable-title__button"
                     , id buttonId
                     , type_ "button"
                     , title config.hint
                     , TestId.attribute config.testId
                     ]
                        ++ (if config.saving then
                                [ attribute "aria-disabled" "true" ]

                            else
                                [ onClick (config.toMsg (Start config.value)) ]
                           )
                    )
                    [ text config.value ]
        ]


{-| Enter commits and Escape cancels; anything else is typing, and is left to
the field.
-}
keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map3 (\key composing keyCode -> ( key, composing || keyCode == 229 ))
        (Decode.field "key" Decode.string)
        (Decode.oneOf [ Decode.field "isComposing" Decode.bool, Decode.succeed False ])
        (Decode.oneOf [ Decode.field "keyCode" Decode.int, Decode.succeed 0 ])
        |> Decode.andThen
            (\( key, composing ) ->
                case key of
                    "Enter" ->
                        if composing then
                            Decode.fail "composing"

                        else
                            Decode.succeed Commit

                    "Escape" ->
                        Decode.succeed Cancel

                    _ ->
                        Decode.fail key
            )


buttonId : String
buttonId =
    "ui-editable-title"


inputId : String
inputId =
    "ui-editable-title-input"
