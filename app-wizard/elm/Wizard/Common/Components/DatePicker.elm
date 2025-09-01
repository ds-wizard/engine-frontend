module Wizard.Common.Components.DatePicker exposing
    ( datePicker
    , datePickerUtc
    , dateTimePicker
    , invalid
    , onChange
    , timePicker
    , value
    )

import Html exposing (Html)
import Html.Attributes exposing (classList)
import Html.Events
import Json.Decode as D
import Json.Encode as E


datePicker : List (Html.Attribute msg) -> Html msg
datePicker attributes =
    Html.node "date-picker" attributes []


datePickerUtc : List (Html.Attribute msg) -> Html msg
datePickerUtc attributes =
    Html.node "date-picker-utc" attributes []


dateTimePicker : List (Html.Attribute msg) -> Html msg
dateTimePicker attributes =
    Html.node "datetime-picker" attributes []


timePicker : List (Html.Attribute msg) -> Html msg
timePicker attributes =
    Html.node "time-picker" attributes []


invalid : Bool -> Html.Attribute msg
invalid isInvalid =
    classList [ ( "is-invalid", isInvalid ), ( "date-picker-invalid", isInvalid ) ]


onChange : (String -> msg) -> Html.Attribute msg
onChange toMsg =
    Html.Events.on "datePickerChanged" <|
        D.map toMsg <|
            D.at [ "target", "datePickerValue" ] <|
                D.string


value : String -> Html.Attribute msg
value v =
    Html.Attributes.property "datePickerValue" (E.string v)
