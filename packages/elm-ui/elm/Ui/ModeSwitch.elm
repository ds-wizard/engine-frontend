module Ui.ModeSwitch exposing (Option, option, toHtml, toHtmlOnSurface, withTestId)

{-| A switch between two or three ways of looking at the same thing: writing a
document or reading it, a template or what it renders to.

**A radio group, not a row of buttons.** The options are one choice with two
answers, so the one that is chosen says so with `aria-checked` and the group
says what the choice is about. A row of toggle buttons would announce two
unrelated pressed states and leave the relationship between them to be
guessed.

**One Tab stop.** Only the chosen option is in the tab order, and the arrow keys
move to the next one and choose it, as in any radio group. Moving focus takes
the DOM, so the keys are handled by `mode-switch.js`, which the switch marks
itself for.

The markdown editor's own switch is this, drawn by the custom element rather
than from here — the same block, so the two stay the same thing. That is the
whole reason it is a component: it was two copies of one pill before.

Where it sits is its container's business. This is the pill and what is in it.

-}

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (attribute, class, tabindex, type_)
import Html.Events exposing (onClick)
import Ui.TestId as TestId


type Option msg
    = Option { label : String, chosen : Bool, onChoose : msg, testId : Maybe String }


option : { label : String, chosen : Bool, onChoose : msg } -> Option msg
option config =
    Option { label = config.label, chosen = config.chosen, onChoose = config.onChoose, testId = Nothing }


withTestId : String -> Option msg -> Option msg
withTestId testId (Option config) =
    Option { config | testId = Just testId }


{-| `label` names the choice the options are answers to — "Response Item
Template", "How this is shown" — and is what a screen reader reads on the way
in. It is not drawn: the options are.
-}
toHtml : { label : String, options : List (Option msg) } -> Html msg
toHtml =
    view ""


{-| The switch on a plain surface — a toolbar that is the page's own white
rather than a sunken bar.

The pill's ground is the surface it sits on, which is what makes the chosen
option stand out of it. On the same surface it is invisible, and what is left is
two words, one of them darker; so on a plain surface it takes the sunken ground
instead.

-}
toHtmlOnSurface : { label : String, options : List (Option msg) } -> Html msg
toHtmlOnSurface =
    view " ui-mode-switch--on-surface"


view : String -> { label : String, options : List (Option msg) } -> Html msg
view extraClass config =
    let
        -- The chosen option is the tab stop; the first one while none is.
        noneChosen =
            List.all (\(Option option_) -> not option_.chosen) config.options

        tabStop index (Option option_) =
            option_.chosen || (noneChosen && index == 0)
    in
    div
        [ class ("ui-mode-switch" ++ extraClass)
        , attribute "role" "radiogroup"
        , attribute "aria-label" config.label
        , attribute "data-ui-mode-switch" ""
        ]
        (List.indexedMap (\index option_ -> viewOption (tabStop index option_) option_) config.options)


viewOption : Bool -> Option msg -> Html msg
viewOption isTabStop (Option config) =
    button
        ([ type_ "button"
         , class "ui-mode-switch__option"
         , attribute "role" "radio"
         , tabindex
            (if isTabStop then
                0

             else
                -1
            )
         , attribute "aria-checked"
            (if config.chosen then
                "true"

             else
                "false"
            )
         , onClick config.onChoose
         ]
            ++ TestId.attributes config.testId
        )
        [ text config.label ]
