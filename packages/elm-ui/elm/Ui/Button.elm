module Ui.Button exposing
    ( Button
    , danger
    , dangerIconOnly
    , dangerSecondary
    , dangerSubmit
    , external
    , ghost
    , iconOnly
    , iconOnlyLink
    , isPrimary
    , large
    , link
    , linkGhost
    , linkPrimary
    , primary
    , quiet
    , secondary
    , small
    , submit
    , toHtml
    , withAccessibleName
    , withBadge
    , withDescription
    , withDisabled
    , withDisabledReason
    , withDone
    , withExternalNotice
    , withIcon
    , withId
    , withLoading
    , withPressed
    , withTestId
    , withTooltip
    , withTooltipAt
    )

{-| An opaque button with a closed API.

There is no `List (Attribute msg)` escape hatch, and no class string is
exported: a caller cannot override the styling or reimplement the component by
hand. Escape hatches are named one at a time — `withTestId`, not
`withAttributes`.

`iconOnly` requires a label, so an unlabeled icon button is unrepresentable
rather than discouraged.

Three kinds of thing look like a button, and each renders as what it is:

  - an action — `primary`, `secondary`, `danger`, `ghost`, `iconOnly` — is a
    `button type="button"`;
  - navigation — `link`, `linkPrimary`, `linkGhost` — is an `a`, so middle-click,
    copy link and the screen reader's links list all keep working;
  - sending a form — `submit` — is a `button type="submit"`, so Enter in a text
    field sends it, which a click handler cannot give you.

-}

import Html exposing (Html, button, span, text)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, rel, target, type_)
import Html.Events exposing (onClick)
import Maybe.Extra as Maybe
import Ui.Badge as Badge exposing (Badge)
import Ui.Icon as Icon exposing (Icon)
import Ui.TestId as TestId
import Ui.Tooltip as Tooltip


type Button msg
    = Button (Config msg)


type alias Config msg =
    { label : String
    , action : Action msg
    , variant : Variant
    , size : Size
    , icon : Maybe Icon
    , iconOnly : Bool
    , loading : Bool
    , done : Bool
    , disabled : Bool
    , testId : Maybe String
    , elementId : Maybe String
    , accessibleName : Maybe String
    , tooltip : Bool
    , externalNotice : String
    , disabledReason : Maybe { id : String, reason : String }
    , description : Maybe { id : String, text : String }
    , pressed : Maybe Bool
    , badge : Maybe (Badge msg)
    , tooltipPlacement : Tooltip.Placement
    }


type Action msg
    = OnClick msg
    | Href { url : String, external : Bool }
    | Submit


type Variant
    = Primary
    | Secondary
    | Danger
    | DangerSecondary
    | Ghost
    | Quiet
    | QuietDanger


type Size
    = Small
    | Medium
    | Large


base : Variant -> String -> Action msg -> Button msg
base variant label action =
    Button
        { label = label
        , action = action
        , variant = variant
        , size = Medium
        , icon = Nothing
        , iconOnly = False
        , loading = False
        , done = False
        , disabled = False
        , testId = Nothing
        , elementId = Nothing
        , accessibleName = Nothing
        , tooltip = False
        , externalNotice = "(opens in a new tab)"
        , disabledReason = Nothing
        , description = Nothing
        , pressed = Nothing
        , badge = Nothing
        , tooltipPlacement = Tooltip.Below
        }


primary : String -> msg -> Button msg
primary label msg =
    base Primary label (OnClick msg)


secondary : String -> msg -> Button msg
secondary label msg =
    base Secondary label (OnClick msg)


danger : String -> msg -> Button msg
danger label msg =
    base Danger label (OnClick msg)


ghost : String -> msg -> Button msg
ghost label msg =
    base Ghost label (OnClick msg)


{-| A link that looks like a secondary button. Rendered as an anchor, because
navigation and action are different things to a keyboard and to a screen
reader.

The family is `link`, `linkPrimary` and `linkGhost`: the variant is how much
the page wants you to go there, and the element is always `a`. "Create" in a
page header navigates to a form, so it is `linkPrimary`, not a primary button
whose handler pushes a URL.

-}
link : String -> String -> Button msg
link label url =
    base Secondary label (Href { url = url, external = False })


{-| A link that looks like a primary button. See `link`.
-}
linkPrimary : String -> String -> Button msg
linkPrimary label url =
    base Primary label (Href { url = url, external = False })


{-| A link that looks like a ghost button. See `link`.
-}
linkGhost : String -> String -> Button msg
linkGhost label url =
    base Ghost label (Href { url = url, external = False })


{-| Opens a link-button in a new tab.

A new tab is a change of context the user did not ask for, so it is announced
(WCAG 3.2.5): a trailing icon for the eye, and a visually hidden
"(opens in a new tab)" in the accessible name for everyone else. A button whose
own icon is already that mark, `fa-arrow-up-right-from-square`, keeps it at the
start and gets no second one.
`rel="noopener noreferrer"` stops the opened page reaching back through
`window.opener`.

The notice defaults to English, like the rest of the system's words it is the
caller's — `withExternalNotice` replaces it in the language of the page.

Has no effect on anything that is not a link.

-}
external : Button msg -> Button msg
external (Button config) =
    case config.action of
        Href hrefConfig ->
            Button { config | action = Href { hrefConfig | external = True } }

        _ ->
            Button config


{-| The words `external` adds to the accessible name, in the page's language.
-}
withExternalNotice : String -> Button msg -> Button msg
withExternalNotice notice (Button config) =
    Button { config | externalNotice = notice }


{-| The button that sends a form: primary, `type="submit"`, and no click
handler — the form's own `onSubmit` is the one place the message is sent from,
so Enter in a field and a click on the button cannot disagree or both fire.

Loading and disabled both stop it, and because a disabled submit button also
blocks implicit submission, a form that is already sending cannot be sent again
by pressing Enter.

Its test hook is `form-submit` unless `withTestId` names a more specific one.

-}
submit : String -> Button msg
submit label =
    base Primary label Submit
        |> withTestId "form-submit"


{-| `submit`, for a form whose submission destroys something — the dialog that
asks for a name to be typed before a package is deleted.

A form modal's submit was primary whatever it did, on the argument that the
label already says it. The label does say it, and a red button says it again
before the last press: these are the dialogs where being sure matters most, and
the one button in them that destroys should not be the same color as Save.

-}
dangerSubmit : String -> Button msg
dangerSubmit label =
    base Danger label Submit
        |> withTestId "form-submit"


{-| An icon-only button. The label is not optional: it becomes the accessible
name, and the visible text is dropped.
-}
iconOnly : Icon -> String -> msg -> Button msg
iconOnly icon label msg =
    let
        (Button config) =
            base Secondary label (OnClick msg)
    in
    Button { config | icon = Just icon, iconOnly = True }


{-| A destructive action that is one of many on the screen — a Delete on every
row of a table, a Remove beside each item of a form — rather than the one
decision a page or a dialog is asking for.

`secondary`'s shape, border and ground, with the label and icon in the danger
role, and a danger tint on hover. It still reads as destructive at a glance, but
a column of them does not shout over the content the way a column of filled red
buttons does. The filled `danger` is for the moment the destruction is
confirmed: the button in the dialog that asks "Delete this?".

The color is not the only signal (1.4.1): the label, or the icon and the name,
say what it does.

-}
dangerSecondary : String -> msg -> Button msg
dangerSecondary label msg =
    base DangerSecondary label (OnClick msg)


{-| `iconOnly` for a destructive action — "Delete" in a table's actions column —
in the `dangerSecondary` style, because a destructive action shown as an icon is
almost always one of a column of them.
-}
dangerIconOnly : Icon -> String -> msg -> Button msg
dangerIconOnly icon label msg =
    let
        (Button config) =
            base DangerSecondary label (OnClick msg)
    in
    Button { config | icon = Just icon, iconOnly = True }


{-| An icon-only link: `iconOnly`'s look, an `a`'s behavior. The label comes
first and the url second, as in `link`, and the label is not optional for the
same reason — it is the link's accessible name, and what a screen reader's list
of links shows for it. `external` adds the new-tab notice to that name.
-}
iconOnlyLink : Icon -> String -> String -> Button msg
iconOnlyLink icon label url =
    let
        (Button config) =
            base Secondary label (Href { url = url, external = False })
    in
    Button { config | icon = Just icon, iconOnly = True }


withIcon : Icon -> Button msg -> Button msg
withIcon icon (Button config) =
    Button { config | icon = Just icon }


{-| Whether this is the primary action of where it is shown — for a container
that repeats it elsewhere, as a list repeats its own in its empty state.
-}
isPrimary : Button msg -> Bool
isPrimary (Button config) =
    config.variant == Primary


{-| Busy rather than unavailable: the button keeps its color and its accessible
name — the label is still in the DOM, only transparent — and says `aria-busy`.
-}
withLoading : Bool -> Button msg -> Button msg
withLoading loading (Button config) =
    Button { config | loading = loading }


{-| Done: what the button started went through. The label gives way to a check,
for the moment before whatever holds the button goes — the unsaved bar after a
save. Disabled, as while loading, so it is not pressed again on the way out.
-}
withDone : Bool -> Button msg -> Button msg
withDone done (Button config) =
    Button { config | done = done }


{-| A button is `disabled`. A link cannot be, so a disabled or loading link loses
its `href` and says `aria-disabled` instead: without an `href` it is not in the
tab order and cannot be followed, which is what a disabled button does too.
-}
withDisabled : Bool -> Button msg -> Button msg
withDisabled isDisabled (Button config) =
    Button { config | disabled = isDisabled }


small : Button msg -> Button msg
small (Button config) =
    Button { config | size = Small }


large : Button msg -> Button msg
large (Button config) =
    Button { config | size = Large }


{-| A fuller name than the visible label, for a button repeated down a list or a
table: every row's "Delete" says "Delete" on the screen, and a screen reader's
list of buttons would otherwise hold a column of identical entries.

The name must begin with the visible label — "Delete API key Laptop" for a button
that shows "Delete" — so someone who operates the page by voice can still say what
they see (WCAG 2.5.3). On an icon-only button the label is what its tooltip shows,
so the name begins with that instead.

-}
withAccessibleName : String -> Button msg -> Button msg
withAccessibleName name (Button config) =
    Button { config | accessibleName = Just name }


{-| Shows an icon-only button's label in a tooltip, on hover and on keyboard
focus, so what the icon means is not left to the icon. It opens below the button,
where tooltips open throughout the system (`withTooltipAt` for elsewhere). Has no effect on a button
whose label is visible.

The bubble is drawn by the button itself — its label in a `data-ui-tooltip`
attribute, shown by the stylesheet — and not by a `Ui.Tooltip` wrapped round it.
It looks and behaves the same (placement, the delay, hoverable and dismissible
with Escape, WCAG 1.4.13), and it is visual only either way: the label is already
the button's name, so the bubble is not announced. What it saves is two elements
per button, which on a page of questions with actions on every one is a few
thousand elements to build each time a chapter opens. A tooltip that describes
its trigger — `withDisabledReason` — stays a real element, because assistive
technology has to reach it.

-}
withTooltip : Button msg -> Button msg
withTooltip (Button config) =
    Button { config | tooltip = True }


{-| `withTooltip`, opening elsewhere than below the button's middle: below and
aligned to its end edge (`End`), for a button at the end of a row, where centered
would run off the side; aligned to its start edge (`Start`), for one at the
start of a row, where centered runs off the other side; or above (`Above`),
where below has no room.
-}
withTooltipAt : Tooltip.Placement -> Button msg -> Button msg
withTooltipAt placement (Button config) =
    Button { config | tooltip = True, tooltipPlacement = placement }


{-| No border and no fill, in the text's own color, with a state layer on hover:
a button among the controls of a dense toolbar, where a row of outlined pills
would outweigh what the toolbar is about. Pressed (`withPressed`), it takes the
selected tint and the brand's ink like any other toggle. Works on any button,
labeled or icon-only; a destructive one (`dangerIconOnly`, `danger`) keeps the
danger ink, with the danger tint on hover.
-}
quiet : Button msg -> Button msg
quiet (Button config) =
    Button
        { config
            | variant =
                case config.variant of
                    Danger ->
                        QuietDanger

                    DangerSecondary ->
                        QuietDanger

                    _ ->
                        Quiet
        }


{-| Unavailable, and saying why — a Delete on a role that is still assigned to
users.

A `disabled` button cannot take focus, so a tooltip on it is reachable by a
pointer and nothing else, and the reason would be hidden from a keyboard and a
screen reader. This one is `aria-disabled` instead: it stays in the tab order,
is announced as unavailable, does nothing when pressed, and looks like a
disabled button. The reason is a tooltip on hover and on focus, and the
button's description (`aria-describedby`), so it is read out with it. `id` is
the tooltip's and has to be unique on the page.

The tooltip opens below the button, aligned to its end. In a table's actions
column every tooltip opens beside the button instead, so none of them makes the
table's scrolling wrapper scroll (`_table.scss`).

-}
withDisabledReason : { id : String, reason : String } -> Button msg -> Button msg
withDisabledReason reason (Button config) =
    Button { config | disabledReason = Just reason }


{-| What the button is for, beyond its label — Download POT, which is to
translate from. It is a tooltip on hover and on focus, and the button's
description (`aria-describedby`), so it is read out with the label. `id` is the
tooltip's and has to be unique on the page. A `withDisabledReason` takes its
place while the button is unavailable.

The tooltip opens below the button, aligned to its end.

-}
withDescription : { id : String, text : String } -> Button msg -> Button msg
withDescription description (Button config) =
    Button { config | description = Just description }


{-| A toggle: a button that stays pressed while what it turns on is on — a side
panel, a filter. `aria-pressed` says so to assistive technology and the
stylesheet shows it with the selected tint, so the state is never color alone
on a button that also says what it is.

The label must stay the same pressed or not ("Search", not "Close search"):
the pressed state is what changes, and a name that changed with it would say the
state twice and the action never.

-}
withPressed : Bool -> Button msg -> Button msg
withPressed pressed (Button config) =
    Button { config | pressed = Just pressed }


{-| A count or a status after the label — "Unanswered 12". It is part of the
button's name, so give the badge a description that says what the number is
(`Badge.withDescription`).
-}
withBadge : Badge msg -> Button msg -> Button msg
withBadge badge (Button config) =
    Button { config | badge = Just badge }


withTestId : String -> Button msg -> Button msg
withTestId testId (Button config) =
    Button { config | testId = Just testId }


{-| An id on the button itself, for the times something else has to name it:
moving focus to it, or pointing an `aria-controls` at it. It is not the test
hook — that is `withTestId`, and the two are separate so a test hook can be
renamed without breaking a reference to the element.
-}
withId : String -> Button msg -> Button msg
withId elementId (Button config) =
    Button { config | elementId = Just elementId }


toHtml : Button msg -> Html msg
toHtml (Button config) =
    let
        classes =
            classList
                [ ( "ui-button", True )
                , ( variantClass config.variant, True )
                , ( "ui-button--sm", config.size == Small )
                , ( "ui-button--lg", config.size == Large )
                , ( "ui-button--icon", config.iconOnly )
                , ( "ui-button--loading", config.loading && not config.done )
                , ( "ui-button--done", config.done )
                ]

        testIdAttrs =
            TestId.attributes config.testId
                ++ (case config.elementId of
                        Just elementId ->
                            [ id elementId ]

                        Nothing ->
                            []
                   )

        -- An icon-only button's label becomes its accessible name; a labeled
        -- button already has one from its text.
        nameAttrs =
            case ( config.accessibleName, config.iconOnly ) of
                ( Just name, _ ) ->
                    [ attribute "aria-label" (name ++ externalNotice) ]

                ( Nothing, True ) ->
                    [ attribute "aria-label" (config.label ++ externalNotice) ]

                ( Nothing, False ) ->
                    []

        withOptionalTooltip element =
            case config.disabledReason of
                Just reason ->
                    Tooltip.viewDescribing { id = reason.id, label = reason.reason, placement = Tooltip.End } element

                Nothing ->
                    case config.description of
                        Just description ->
                            Tooltip.viewDescribing { id = description.id, label = description.text, placement = Tooltip.End } element

                        Nothing ->
                            element

        -- An icon-only button's tooltip, drawn from this attribute by the
        -- stylesheet (`withTooltip`).
        tooltipAttrs =
            if config.tooltip && config.iconOnly && Maybe.isNothing config.disabledReason && Maybe.isNothing config.description then
                attribute "data-ui-tooltip" config.label
                    :: (case config.tooltipPlacement of
                            Tooltip.Above ->
                                [ attribute "data-ui-tooltip-placement" "above" ]

                            Tooltip.End ->
                                [ attribute "data-ui-tooltip-placement" "end" ]

                            Tooltip.Start ->
                                [ attribute "data-ui-tooltip-placement" "start" ]

                            _ ->
                                []
                       )

            else
                []

        -- Unavailable with a reason: focusable and described, never pressed.
        reasonAttrs =
            case config.disabledReason of
                Just reason ->
                    [ attribute "aria-disabled" "true", attribute "aria-describedby" reason.id ]

                Nothing ->
                    case config.description of
                        Just description ->
                            [ attribute "aria-describedby" description.id ]

                        Nothing ->
                            []

        pressedAttrs =
            case config.pressed of
                Just True ->
                    [ attribute "aria-pressed" "true" ]

                Just False ->
                    [ attribute "aria-pressed" "false" ]

                Nothing ->
                    []

        busyAttrs =
            if config.loading && not config.done then
                [ attribute "aria-busy" "true" ]

            else
                []

        externalNotice =
            if isExternal then
                " " ++ config.externalNotice

            else
                ""

        isExternal =
            case config.action of
                Href hrefConfig ->
                    hrefConfig.external

                _ ->
                    False

        -- Only the parts the button has: an empty text node standing in for a
        -- missing icon, label, notice or spinner is still a node to create and
        -- to diff, and a page of questions has thousands of buttons.
        content =
            Maybe.values
                [ Maybe.map Icon.toHtml config.icon
                , if config.iconOnly then
                    Nothing

                  else
                    Just (span [] [ text config.label ])
                , Maybe.map Badge.toHtml config.badge

                -- The mark that says this leaves the page, after the label.
                -- Not on an icon-only link: that is a control the size of one
                -- icon, its icon is the caller's, and a second one beside it is
                -- two glyphs in a round button — often the same glyph twice,
                -- since the arrow is the obvious icon for "open this". What the
                -- mark says in words is in the link's `aria-label` either way.
                -- Nor the same glyph after a label it already leads: there only
                -- its words are added.
                , if isExternal && not config.iconOnly then
                    let
                        externalMark =
                            Icon.solid "fa-arrow-up-right-from-square"
                    in
                    if config.icon == Just externalMark then
                        Just (span [ class "ui-visually-hidden" ] [ text externalNotice ])

                    else
                        Just
                            (span [ class "ui-button__external" ]
                                [ Icon.toHtml externalMark
                                , span [ class "ui-visually-hidden" ] [ text externalNotice ]
                                ]
                            )

                  else
                    Nothing
                , if config.done then
                    Just (span [ class "ui-button__done" ] [ Icon.toHtml (Icon.solid "fa-check") ])

                  else if config.loading then
                    Just (span [ class "ui-button__spinner" ] [ Icon.toHtml Icon.spinner ])

                  else
                    Nothing
                ]

        buttonElement typeName clickAttrs =
            button
                (classes
                    :: type_ typeName
                    :: disabled (config.disabled || config.loading || config.done)
                    :: clickAttrs
                    ++ nameAttrs
                    ++ busyAttrs
                    ++ pressedAttrs
                    ++ reasonAttrs
                    ++ tooltipAttrs
                    ++ testIdAttrs
                )
                content

        hasReason =
            Maybe.isJust config.disabledReason
    in
    withOptionalTooltip <|
        case config.action of
            OnClick msg ->
                if hasReason then
                    buttonElement "button" []

                else
                    buttonElement "button" [ onClick msg ]

            Submit ->
                -- A plain button while unavailable, so Enter in the form does
                -- not submit through it.
                if hasReason then
                    buttonElement "button" []

                else
                    buttonElement "submit" []

            Href hrefConfig ->
                let
                    linkAttrs =
                        if hasReason then
                            -- No href, so it cannot be followed; the tabindex
                            -- keeps it where its reason can be reached.
                            [ attribute "role" "link", attribute "tabindex" "0" ]

                        else if config.disabled || config.loading || config.done then
                            [ attribute "role" "link", attribute "aria-disabled" "true" ]

                        else if hrefConfig.external then
                            [ href hrefConfig.url, target "_blank", rel "noopener noreferrer" ]

                        else
                            [ href hrefConfig.url ]
                in
                Html.a
                    (classes :: linkAttrs ++ nameAttrs ++ busyAttrs ++ reasonAttrs ++ tooltipAttrs ++ testIdAttrs)
                    content


variantClass : Variant -> String
variantClass variant =
    case variant of
        Primary ->
            "ui-button--primary"

        Secondary ->
            "ui-button--secondary"

        Danger ->
            "ui-button--danger"

        DangerSecondary ->
            "ui-button--danger-secondary"

        Quiet ->
            "ui-button--quiet"

        QuietDanger ->
            "ui-button--quiet ui-button--quiet-danger"

        Ghost ->
            "ui-button--ghost"
