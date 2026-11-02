module Common.Components.Tour exposing
    ( Config
    , Step
    , byField
    , byId
    , byTestId
    , centered
    , mainContent
    , mainHeading
    , navigation
    , onDone
    , tour
    , tourInModal
    )

{-| Guided tours: a sequence of popovers, each pointing at part of the page.

The engine is driver.js; what matters is what driver.js is told and how its
popover looks (`packages/elm-common/js/ports/tour.js`,
`packages/elm-common/scss/components/_tour.scss`):

  - **The popover is a dialog that takes focus.** driver.js already renders
    `role="dialog"` named by the title and described by the text, and traps Tab
    inside it. Focus goes to the Next (or Done) button on every step, so a
    screen reader reads the dialog's name and text and lands on the thing to
    press, and Enter continues.
  - **The step is said in words.** "Step 2 of 5", translated, and part of the
    dialog's description rather than a floating "2/5".
  - **Escape ends it; the arrow keys move.** A tour skipped part-way asks first,
    and the confirmation says tours can be reset.
  - **Focus goes back where it was.** When the tour ends, focus returns to the
    element that had it before; if the page has re-rendered that element away,
    to the page heading.
  - **It follows the theme.** Surfaces, text, radius and the focus ring are
    roles, the buttons are `Ui.Button`'s own classes, and the overlay is the
    modal backdrop, so dark mode needs nothing extra.
  - **Reduced motion is respected.** No animation between steps when the reader
    has asked for less.


## Pointing at things

Steps select elements with a CSS selector, and there are no attributes added
for tours. The selectors use what the page already has, which keeps a tour from
being the only reason markup exists and from breaking silently when it is
removed:

  - `byId` — any element the design system gives an id: a field's control, a
    navigation item, a menu.
  - `byField` — a whole form field (label, control, hint) by its control's id.
  - `byTestId` — a `Ui.Button.withTestId`, which is already the stable name
    end-to-end tests use.
  - `navigation`, `mainContent`, `mainHeading` — the shell's landmarks.
  - `centered` — no element: the popover sits in the middle of the screen, for
    the introduction.

The first step's element is waited for (up to five seconds) before the tour
starts, so a tour started from a page's `init` does not begin before the page is
drawn.

-}

import Common.Data.Context exposing (WithLocale)
import Common.Ports.Tour as Ports
import Gettext exposing (gettext)
import Json.Encode as E
import Json.Encode.Extra as E
import String.Format as String
import Ui.AppShell as AppShell


type alias Step =
    { element : Maybe String
    , title : String
    , description : String
    }


type alias Config =
    { tourId : String
    , completedTours : List String
    , enabled : Bool
    , steps : List Step
    }


{-| Start the tour, unless tours are off, it was already completed, or it has
no steps. `enabled` is the application's whole decision — the tours feature is
on and someone is signed in to record completion against.
-}
tour : WithLocale a -> Config -> Cmd msg
tour appState config =
    start appState 0 config


{-| The same, for a tour inside a modal: it waits for the dialog's entrance so
the first popover is placed against where the element ends up, not where it
started.
-}
tourInModal : WithLocale a -> Config -> Cmd msg
tourInModal appState config =
    start appState 200 config


start : WithLocale a -> Int -> Config -> Cmd msg
start appState delay config =
    if not config.enabled || List.member config.tourId config.completedTours || List.isEmpty config.steps then
        Cmd.none

    else
        Ports.drive (encode appState delay config)


{-| The id of a tour that has ended, finished or skipped — to record it as
completed.
-}
onDone : (String -> msg) -> Sub msg
onDone =
    Ports.onDone


encode : WithLocale a -> Int -> Config -> E.Value
encode appState delay config =
    E.object
        [ ( "tourId", E.string config.tourId )
        , ( "steps", E.list encodeStep config.steps )
        , ( "delay", E.int delay )
        , ( "skipTourText"
          , E.string
                (String.format "%s\n(%s)"
                    [ gettext "Are you sure you want to skip the tour?" appState.locale
                    , gettext "You can reset tours in user settings" appState.locale
                    ]
                )
          )
        , ( "nextBtnText", E.string (gettext "Next" appState.locale) )
        , ( "prevBtnText", E.string (gettext "Previous" appState.locale) )
        , ( "doneBtnText", E.string (gettext "Done" appState.locale) )
        , ( "closeBtnText", E.string (gettext "Close" appState.locale) )

        -- driver.js fills in the numbers.
        , ( "progressText", E.string (String.format (gettext "Step %s of %s" appState.locale) [ "{{current}}", "{{total}}" ]) )
        , ( "fallbackFocus", E.string ("#" ++ AppShell.mainHeadingId) )
        ]


encodeStep : Step -> E.Value
encodeStep step =
    E.object
        [ ( "element", E.maybe E.string step.element )
        , ( "popover"
          , E.object
                [ ( "title", E.string step.title )
                , ( "description", E.string step.description )
                ]
          )
        ]



-- SELECTORS


{-| No element: the popover is centered over the page.
-}
centered : Maybe String
centered =
    Nothing


byId : String -> Maybe String
byId id =
    Just ("#" ++ id)


{-| The whole field — label, control and hint — around the control with this
id, so the highlight covers what the step is talking about rather than only the
box you type into.
-}
byField : String -> Maybe String
byField id =
    Just (".ui-field:has(#" ++ id ++ ")")


{-| An element given a test id — `Ui.Button.withTestId` renders it as
`data-cy`.
-}
byTestId : String -> Maybe String
byTestId testId =
    Just ("[data-cy=\"" ++ testId ++ "\"]")


navigation : Maybe String
navigation =
    Just "#ui-nav"


mainContent : Maybe String
mainContent =
    Just "#ui-main"


mainHeading : Maybe String
mainHeading =
    byId AppShell.mainHeadingId
