module Shared.Components.Tooltip exposing
    ( tooltip
    , tooltipCustom
    , tooltipLeft
    , tooltipRight
    )

import Html
import Html.Attributes exposing (attribute, class)


tooltip : String -> List (Html.Attribute msg)
tooltip =
    tooltipCustom ""


tooltipLeft : String -> List (Html.Attribute msg)
tooltipLeft =
    tooltipCustom "with-tooltip-left"


tooltipRight : String -> List (Html.Attribute msg)
tooltipRight =
    tooltipCustom "with-tooltip-right"


tooltipCustom : String -> String -> List (Html.Attribute msg)
tooltipCustom extraClass value =
    [ class "with-tooltip", class extraClass, attribute "data-tooltip" value ]
