module Common.Components.SavingIndicator exposing
    ( Model
    , Msg
    , init
    , isSaving
    , saved
    , saving
    , subscriptions
    , toHtml
    , update
    )

{-| Where the user's own changes are: on their way to the server, arrived a
moment ago, or all arrived.

Written inline on the project page first; the knowledge model editor needs the
same three states and the same three-second countdown, so it lives here rather
than twice.

**A status, not a control.** It is `role="status"`, so a change is said politely
rather than interrupting, and it is never something to press.

**"Saved" fades, "all saved" stays.** For a few seconds after the last change
lands the indicator says so in words; after that it is a tick with its meaning
in a tooltip and a visually hidden phrase, because a permanent "Saved" beside a
heading reads as a button that failed.

-}

import Gettext exposing (gettext)
import Html exposing (Html, span, text)
import Html.Attributes exposing (attribute, class)
import Time
import Ui.Icon as Icon
import Ui.Text as Text
import Ui.Tooltip as Tooltip


type Model
    = Saving
    | SavedRecently Int
    | AllSaved


init : Model
init =
    AllSaved


{-| Something is on its way. A page is in this state from the moment a change is
made — typing still waiting for the debounce is a change not yet sent, and so
not yet saved.
-}
saving : Model
saving =
    Saving


{-| The last change has landed.
-}
saved : Model
saved =
    SavedRecently secondsToSayItFor


{-| True while anything is unsaved, which is what a page's unsaved-changes guard
asks.
-}
isSaving : Model -> Bool
isSaving model =
    model == Saving


secondsToSayItFor : Int
secondsToSayItFor =
    3


type Msg
    = Tick


update : Msg -> Model -> Model
update Tick model =
    case model of
        SavedRecently seconds ->
            if seconds <= 1 then
                AllSaved

            else
                SavedRecently (seconds - 1)

        _ ->
            model


{-| A tick a second, and only while there is a countdown to run down.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        SavedRecently _ ->
            Time.every 1000 (always Tick)

        _ ->
            Sub.none


toHtml : { a | locale : Gettext.Locale } -> Model -> Html msg
toHtml appState model =
    span [ class "c-project-saving", attribute "role" "status" ]
        (case model of
            Saving ->
                [ Icon.toHtml Icon.spinner, text (gettext "Saving..." appState.locale) ]

            SavedRecently _ ->
                [ Icon.toHtml (Icon.solid "fa-check"), text (gettext "Saved" appState.locale) ]

            AllSaved ->
                [ Tooltip.view { label = gettext "All changes have been saved" appState.locale, placement = Tooltip.Below }
                    (span [] [ Icon.toHtml (Icon.solid "fa-check") ])
                , Text.visuallyHiddenInline (gettext "All changes have been saved" appState.locale)
                ]
        )
