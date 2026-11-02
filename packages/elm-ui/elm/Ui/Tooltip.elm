module Ui.Tooltip exposing
    ( Placement(..)
    , view
    , viewDescribing
    )

{-| A tooltip that works from the keyboard.

A tooltip drawn from a `data-tooltip` attribute through `::after`, on `:hover`
only, has two problems: pseudo-element content is not reliably exposed to
assistive technology, and a keyboard user never sees it at all.

Here the bubble is a real element with `role="tooltip"`, and it appears on hover
**and** focus.

There is no arrow. At this distance the position carries the association on its
own, and an arrow would need aiming at the trigger independently of the bubble
whenever the bubble is edge-aligned — which is exactly when it is hardest. The
existing implementation does keep them separate, and it works, but it is two
things to hold in sync for something that adds no information. If a `Popover`
lands later — larger, further from its anchor, possibly repositioned — that is
where a pointer earns its place.

Two variants, and the distinction matters:

  - `viewDescribing` associates the bubble with the trigger through
    `aria-describedby`. Use it when the tooltip says something the trigger's own
    name does not.
  - `view` marks the bubble `aria-hidden`. Use it for an icon-only button, whose
    label is already its accessible name — describing it as well makes a screen
    reader say the same words twice.

-}

import Html exposing (Html, span, text)
import Html.Attributes exposing (attribute, class, classList, id)


type Placement
    = Above
    | Below
    | Start
    | End
      -- Beside the trigger, not under it: for an icon-only control in a column
      -- of them, where under it is the next control.
    | Trailing


{-| Visual only. The trigger must already carry its own accessible name.
-}
view : { label : String, placement : Placement } -> Html msg -> Html msg
view config trigger =
    span [ class "ui-tooltip" ]
        [ trigger
        , bubble config.placement config.label [ attribute "aria-hidden" "true" ]
        ]


{-| Adds the tooltip to the trigger's description. The caller passes the id it
put on the trigger's `aria-describedby`.
-}
viewDescribing : { id : String, label : String, placement : Placement } -> Html msg -> Html msg
viewDescribing config trigger =
    span [ class "ui-tooltip" ]
        [ trigger
        , bubble config.placement config.label [ id config.id ]
        ]


bubble : Placement -> String -> List (Html.Attribute msg) -> Html msg
bubble placement label attrs =
    span
        (attribute "role" "tooltip"
            :: classList
                [ ( "ui-tooltip__bubble", True )
                , ( "ui-tooltip__bubble--above", placement == Above )
                , ( "ui-tooltip__bubble--below", placement == Below )
                , ( "ui-tooltip__bubble--start", placement == Start )
                , ( "ui-tooltip__bubble--end", placement == End )
                , ( "ui-tooltip__bubble--trailing", placement == Trailing )
                ]
            :: attrs
        )
        [ text label ]
