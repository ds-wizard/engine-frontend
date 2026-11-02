module Ui.Resizer exposing
    ( Config
    , Edge(..)
    , Model
    , Msg
    , Words
    , dragDone
    , dragFrom
    , dragTo
    , init
    , isCollapsed
    , isDragging
    , nudgeBy
    , restore
    , setCollapsed
    , setWidth
    , subscriptions
    , update
    , view
    , viewWithWords
    , widthPx
    )

{-| A draggable boundary between a panel and the content beside it.

Two of them exist: the navigation's trailing edge and the detail page's side
panel. They are the same machine — a width, a pair of bounds, a pointer delta
and a set of keys — so they are one module rather than two implementations that
drift.

It is the APG window splitter: a focusable `separator` carrying its own value,
where dragging is the obvious way to use it and cannot be the only way. WCAG
2.5.7 wants the same result without a drag and 2.1.1 wants it from the keyboard
at all, so the arrow keys move the edge, Home and End take it to its limits,
Enter collapses a panel that can collapse, Escape abandons a drag in progress
and a double-click puts it back where it started.

**Not `packages/elm-vendor/SplitPane.elm`.** That is the right tool for the editor
panes it already serves and the wrong one here: it owns the markup of both sides
— which fights the layouts these panels live in — and its splitter is mouse-only,
with no role, no value and no keyboard.

-}

import Browser.Events
import Html exposing (Html, div)
import Html.Attributes exposing (attribute, classList, tabindex)
import Html.Events
import Json.Decode as Decode
import Maybe.Extra as Maybe


{-| Which edge of the panel the separator sits on, which is also the direction
the panel grows in. A navigation at the start of the page is resized from its
`Trailing` edge; a panel at the end of it from its `Leading` edge, where moving
the pointer right makes the panel _narrower_.
-}
type Edge
    = Trailing
    | Leading


type alias Config =
    { edge : Edge
    , label : String
    , min : Float
    , max : Float
    , default : Float

    -- A panel that can get out of the way entirely. `at` is the width below
    -- which dragging stops fighting the minimum and collapses instead:
    -- dragging a sidebar as far as it goes means "not now", and a panel that
    -- stops short of that reads as broken.
    , collapse : Maybe { at : Float, rail : Float }
    }


type Model
    = Model
        { config : Config
        , width : Float
        , collapsed : Bool
        , drag : Maybe Drag
        }


type alias Drag =
    { startX : Float
    , startWidth : Float
    }


{-| Widths are in pixels, deliberately, where everything else in the system is
in rem.

A width someone dragged is a size they chose against what was on the screen in
front of them. Re-scaling it with the root font size would move the boundary out
from under the pointer that set it — the text inside still scales, which is the
part 1.4.4 is about.

-}
init : Config -> Model
init config =
    Model
        { config = config
        , width = config.default
        , collapsed = False
        , drag = Nothing
        }


{-| Rebuild what the app stored. Both parts are clamped on the way in: a stored
width outlives the release that changes the bounds.
-}
restore : { collapsed : Bool, width : Maybe Float } -> Model -> Model
restore stored (Model model) =
    Model
        { model
            | width = clampWidth model.config (Maybe.withDefault model.config.default stored.width)
            , collapsed = stored.collapsed && Maybe.isJust model.config.collapse
            , drag = Nothing
        }


isCollapsed : Model -> Bool
isCollapsed (Model model) =
    model.collapsed


widthPx : Model -> Float
widthPx (Model model) =
    model.width


{-| True while the pointer is down on the separator.

For the app's storage: persisting on every pointer move would write sixty values
to learn the one it ends on.

-}
isDragging : Model -> Bool
isDragging (Model model) =
    Maybe.isJust model.drag



-- UPDATE


type Msg
    = SetCollapsed Bool
    | DragStarted Float
    | DragMoved Float
    | DragEnded
    | DragCanceled
    | Nudged Float
    | Resized Float


{-| The messages, as verbs.

The view produces all of these on its own — an app that only renders a panel
never needs them. They are here because an app sometimes has an opinion of its
own (collapse below a breakpoint, a "reset width" in a menu), and because the
drag arithmetic is the part of this module most worth a test and there is no
other way to reach it.

-}
setCollapsed : Bool -> Msg
setCollapsed =
    SetCollapsed


setWidth : Float -> Msg
setWidth =
    Resized


nudgeBy : Float -> Msg
nudgeBy =
    Nudged


dragFrom : Float -> Msg
dragFrom =
    DragStarted


dragTo : Float -> Msg
dragTo =
    DragMoved


dragDone : Msg
dragDone =
    DragEnded


update : Msg -> Model -> Model
update msg (Model model) =
    case msg of
        SetCollapsed collapsed ->
            Model { model | collapsed = collapsed && Maybe.isJust model.config.collapse }

        DragStarted x ->
            Model { model | drag = Just { startX = x, startWidth = effectiveWidth (Model model) } }

        DragMoved x ->
            case model.drag of
                Just drag ->
                    -- The delta, not the pointer's own position: the separator
                    -- is grabbed wherever it was grabbed and stays there. On
                    -- the leading edge the sign flips, because a panel at the
                    -- end of the page grows when the pointer moves towards the
                    -- start of it.
                    resizeTo (drag.startWidth + towards model.config.edge (x - drag.startX)) (Model model)

                Nothing ->
                    Model model

        DragEnded ->
            Model { model | drag = Nothing }

        DragCanceled ->
            case model.drag of
                Just drag ->
                    Model { model | drag = Nothing, width = drag.startWidth, collapsed = False }

                Nothing ->
                    Model model

        Nudged delta ->
            if model.collapsed then
                -- Widening a collapsed panel opens it; narrowing it further has
                -- nowhere to go.
                if delta > 0 then
                    Model { model | collapsed = False }

                else
                    Model model

            else
                resizeTo (model.width + delta) (Model model)

        Resized width ->
            Model { model | collapsed = False, width = clampWidth model.config width }


{-| One place where a width becomes a state, so dragging and the arrow keys
cannot disagree about when a panel collapses.
-}
resizeTo : Float -> Model -> Model
resizeTo width (Model model) =
    case model.config.collapse of
        Just collapse ->
            if width < collapse.at then
                Model { model | collapsed = True }

            else
                Model { model | collapsed = False, width = clampWidth model.config width }

        Nothing ->
            Model { model | width = clampWidth model.config width }


clampWidth : Config -> Float -> Float
clampWidth config =
    clamp config.min config.max


towards : Edge -> Float -> Float
towards edge delta =
    case edge of
        Trailing ->
            delta

        Leading ->
            -delta


effectiveWidth : Model -> Float
effectiveWidth (Model model) =
    case ( model.collapsed, model.config.collapse ) of
        ( True, Just collapse ) ->
            collapse.rail

        _ ->
            model.width


{-| Only while the pointer is down. A resize that keeps listening after the
mouse comes up is a page that resizes itself when you wave at it.
-}
subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    case model.drag of
        Just _ ->
            Sub.batch
                [ Browser.Events.onMouseMove (Decode.map DragMoved clientX)
                , Browser.Events.onMouseUp (Decode.succeed DragEnded)
                , Browser.Events.onKeyDown (Decode.map (always DragCanceled) escapeKey)
                ]

        Nothing ->
            Sub.none


clientX : Decode.Decoder Float
clientX =
    Decode.field "clientX" Decode.float


escapeKey : Decode.Decoder ()
escapeKey =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if key == "Escape" then
                    Decode.succeed ()

                else
                    Decode.fail "not escape"
            )



-- VIEW


{-| The separator itself. Its parent has to be `position: relative` — it hangs
on the panel's edge rather than taking a column of its own, so that the thing
you grab is the edge you see.

The press is `preventDefault`ed, which is what stops the pointer from selecting
half the page on the way across it. That also means the separator does not take
focus on click, which is the behavior you want: a mouse user who has just
finished dragging is not about to press an arrow key, and stealing focus from
whatever they were doing to say so would be worse.

-}
view : (Msg -> msg) -> Model -> Html msg
view toMsg (Model model) =
    viewWithWords
        { label = model.config.label
        , collapsed = "Collapsed"
        , width = \pixels -> String.fromInt pixels ++ " pixels"
        }
        toMsg
        (Model model)


{-| What the separator says: its name, and its value in words — while collapsed,
and as a width. `view` takes the name from the `Config` and says the value in
English.

Words rather than part of `Config`, because the language can change while the
model lives on, and a model holding a function cannot be compared.

-}
type alias Words =
    { label : String
    , collapsed : String
    , width : Int -> String
    }


{-| `view`, with the words given.
-}
viewWithWords : Words -> (Msg -> msg) -> Model -> Html msg
viewWithWords words toMsg (Model model) =
    let
        width =
            round (effectiveWidth (Model model))

        low =
            case model.config.collapse of
                -- The rail rather than the minimum: collapsing is a value the
                -- separator reaches, and a valuenow below its own valuemin is a
                -- broken control.
                Just collapse ->
                    collapse.rail

                Nothing ->
                    model.config.min
    in
    div
        [ classList
            [ ( "ui-resizer", True )
            , ( "ui-resizer--leading", model.config.edge == Leading )
            , ( "is-dragging", Maybe.isJust model.drag )
            ]
        , attribute "role" "separator"
        , attribute "aria-orientation" "vertical"
        , attribute "aria-label" words.label
        , attribute "aria-valuenow" (String.fromInt width)
        , attribute "aria-valuemin" (String.fromInt (round low))
        , attribute "aria-valuemax" (String.fromInt (round model.config.max))
        , attribute "aria-valuetext"
            (if model.collapsed then
                words.collapsed

             else
                words.width width
            )
        , tabindex 0
        , -- WCAG 2.5.2 down-event: a drag has no other moment to begin. Nothing
          -- completes on the press — the width only moves once the pointer does
          -- — and Escape puts it back where it started.
          Html.Events.preventDefaultOn "mousedown"
            (Decode.map (\x -> ( toMsg (DragStarted x), True )) primaryButtonX)
        , Html.Events.onDoubleClick (toMsg (Resized model.config.default))
        , Html.Events.preventDefaultOn "keydown"
            (Decode.map (\msg -> ( toMsg msg, True )) (resizeKey model.config model.collapsed))
        ]
        []


{-| Only the primary button starts a drag. A right-click opening the context
menu would otherwise leave the panel following the pointer with nothing held
down.
-}
primaryButtonX : Decode.Decoder Float
primaryButtonX =
    Decode.field "button" Decode.int
        |> Decode.andThen
            (\button ->
                if button == 0 then
                    clientX

                else
                    Decode.fail "not the primary button"
            )


{-| The splitter keys, and the same ones people already expect from one.

Arrow keys move the _separator_, not the panel: right is right whichever side of
the page the panel is on, which is why the leading edge inverts them.

Note for RTL: these read the document as left-to-right. Mirroring is a matter of
flipping the sign again, but it needs to know the document direction, which
nothing in the system reads yet (§14).

-}
resizeKey : Config -> Bool -> Decode.Decoder Msg
resizeKey config collapsed =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                case key of
                    "ArrowLeft" ->
                        Decode.succeed (Nudged (towards config.edge -step))

                    "ArrowRight" ->
                        Decode.succeed (Nudged (towards config.edge step))

                    "Home" ->
                        Decode.succeed (Resized config.min)

                    "End" ->
                        Decode.succeed (Resized config.max)

                    "Enter" ->
                        Decode.succeed (SetCollapsed (not collapsed))

                    _ ->
                        Decode.fail "not a resize key"
            )


{-| One arrow press. Coarse enough to be worth pressing, fine enough to land
where you meant.
-}
step : Float
step =
    16
