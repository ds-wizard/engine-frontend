port module Ui.Copy exposing
    ( Config
    , Model
    , Msg
    , Words
    , brief
    , briefWithWords
    , button
    , buttonWithWords
    , init
    , inline
    , inlineWithWords
    , labeledWithWords
    , textButtonWithWords
    , update
    )

{-| Putting a value on the clipboard, and saying so.

Three modules do this today — `Copyable` (internal), `CopyableCodeBlock`
(wizard) and `ItemIdBox` (registry) — over one port, and all three say what is
about to happen and then what happened in the same tooltip. That is the right
place for it visually: the words appear where the pointer already is, and they
appear on focus too, so the keyboard gets the same prompt.

It is not the right place for it _only_. The bubble is `aria-hidden`, because the
button's own name already says what the button does, and the confirmation is
repeated in a live region that is not on the screen — a tooltip is a description,
and "Copied" is the result of pressing, which is a thing to announce rather than a
thing to re-read.

The confirmation clears itself after a moment, because one that stays stops being
about the press that caused it.

-}

import Html exposing (Html, code, div, span, text)
import Html.Attributes exposing (attribute, class, type_)
import Html.Events exposing (onClick)
import Process
import Task
import Ui.Icon as Icon
import Ui.Tooltip as Tooltip


{-| Which value was last copied, so that one button's confirmation does not
appear under every other one on the page.
-}
type Model
    = Model (Maybe String)


init : Model
init =
    Model Nothing


type Msg
    = Requested String String
    | Expired String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        Requested key value ->
            ( Model (Just key)
            , Cmd.batch
                [ uiCopyToClipboard value
                , Process.sleep confirmationFor |> Task.perform (always (Expired key))
                ]
            )

        Expired key ->
            -- Only if it is still the same one: two copies in quick succession
            -- would otherwise have the first one's timer clear the second's
            -- confirmation.
            if model == Just key then
                ( Model Nothing, Cmd.none )

            else
                ( Model model, Cmd.none )


{-| How long "Copied" stays. Long enough to read, short enough that it is
obviously about what just happened.
-}
confirmationFor : Float
confirmationFor =
    2000


port uiCopyToClipboard : String -> Cmd msg


type alias Config msg =
    { key : String
    , label : String
    , value : String
    , model : Model
    , toMsg : Msg -> msg
    }


{-| A button that copies, and nothing else.

`label` names what is being copied — "Copy the identifier" — because a page with
four of these has four buttons called "Copy" otherwise, and position is not
something a screen reader reads out.

-}
button : Config msg -> Html msg
button =
    buttonWithWords english


{-| What the tooltip and the announcement say: `prompt` before the press, `done`
after it. `button` and `inline` say it in English, like every other default in
the system; these are the same components in the language of the page.
-}
type alias Words =
    { prompt : String
    , done : String
    }


english : Words
english =
    { prompt = "Click to copy", done = "Copied" }


{-| `button`, with the words given.
-}
buttonWithWords : Words -> Config msg -> Html msg
buttonWithWords words config =
    span [ class "ui-copy" ]
        [ viewButton words config []
        , viewConfirmation words config
        ]


{-| The button with the first `chars` characters of what it copies inside it.

An identifier is copied to be pasted somewhere else and almost never read, so
printing the whole of a uuid across a heading is noise; the first few characters
are enough to tell one from another and to see that there is one to take. The
whole value is what goes on the clipboard.

-}
brief : Int -> Config msg -> Html msg
brief =
    briefWithWords english


{-| `brief`, with the words given.
-}
briefWithWords : Words -> Int -> Config msg -> Html msg
briefWithWords words chars config =
    span [ class "ui-copy" ]
        [ viewButton words config [ Html.small [ class "ui-copy__brief" ] [ text (String.left chars config.value) ] ]
        , viewConfirmation words config
        ]


{-| A value shown next to the button that copies it: an identifier beside a
label, which is what this is mostly used for and the one case a code block
cannot cover.
-}
inline : Config msg -> Html msg
inline =
    inlineWithWords english


{-| `inline`, with the words given.
-}
inlineWithWords : Words -> Config msg -> Html msg
inlineWithWords words config =
    div [ class "ui-copy ui-copy--inline" ]
        [ code [ class "ui-copy__value" ] [ text config.value ]
        , viewButton words config []
        , viewConfirmation words config
        ]


{-| A ghost button with the copy icon and `label` as its words — "Copy" — for
the end of a header's line, where the guide link goes
(`Ui.Modal.withHeaderControl`). The tooltip is anchored to the button's end and
opens back toward the start, so at the edge of a dialog it stays inside it.
-}
labeledWithWords : Words -> Config msg -> Html msg
labeledWithWords words config =
    let
        (Model copied) =
            config.model
    in
    span [ class "ui-copy" ]
        [ Tooltip.view
            { label =
                if copied == Just config.key then
                    words.done

                else
                    words.prompt
            , placement = Tooltip.End
            }
            (Html.button
                [ type_ "button"
                , class "ui-button ui-button--ghost"
                , onClick (config.toMsg (Requested config.key config.value))
                ]
                [ Icon.toHtml (Icon.solid "fa-copy")
                , span [] [ text config.label ]
                ]
            )
        , viewConfirmation words config
        ]


{-| A text button that copies, and says it did in its own words — "Copy link"
with a link icon, then "Copied!" with a check, and back. The wizard's share
dialog had exactly this under the public link, where the value itself is
nothing to read.

`label` is what it says before the press and `words.done` after; `prompt` is not
used, because the label already says what pressing does. Both icons are fixed
width, so the words stay where they were when the icon changes. The change is
announced from the same live region the other forms use: a button's own text
changing under the focus is not something every screen reader reads out.

-}
textButtonWithWords : Words -> Config msg -> Html msg
textButtonWithWords words config =
    let
        (Model copied) =
            config.model

        ( icon, label ) =
            if copied == Just config.key then
                ( "fa-check", words.done )

            else
                ( "fa-link", config.label )
    in
    span [ class "ui-copy" ]
        [ Html.button
            [ type_ "button"
            , class "ui-copy__text-button"
            , onClick (config.toMsg (Requested config.key config.value))
            ]
            [ Icon.toHtml (Icon.solid (icon ++ ""))
            , span [ class "ui-copy__text-button-label" ] [ text label ]
            ]
        , viewConfirmation words config
        ]


{-| The button, with the tooltip that says what pressing it will do and then what
it did.

`Tooltip.view` rather than `viewDescribing`: the bubble is `aria-hidden`, because
the button's `aria-label` already says this in words that do not change. It shows
on hover and on focus, which is the whole reason it is worth having — the prompt
and the confirmation reach a keyboard as well as a pointer.

-}
viewButton : Words -> Config msg -> List (Html msg) -> Html msg
viewButton words config extra =
    let
        (Model copied) =
            config.model
    in
    Tooltip.view
        { label =
            if copied == Just config.key then
                words.done

            else
                words.prompt
        , placement = Tooltip.Below
        }
        (Html.button
            [ type_ "button"
            , class "ui-copy__button"
            , attribute "aria-label" config.label
            , onClick (config.toMsg (Requested config.key config.value))
            ]
            (Icon.toHtml (Icon.solid "fa-copy") :: extra)
        )


{-| The result of the press, in a live region that is always in the document.

Always there, empty until there is something to say: a region added at the
moment it has content is a region some screen readers never announce.

-}
viewConfirmation : Words -> Config msg -> Html msg
viewConfirmation words config =
    let
        (Model copied) =
            config.model
    in
    span
        [ -- Off the screen: what it says is on it already, in the tooltip. This
          -- is the copy of it that gets announced.
          class "ui-copy__confirmation ui-visually-hidden"
        , attribute "role" "status"
        ]
        [ if copied == Just config.key then
            text words.done

          else
            text ""
        ]
