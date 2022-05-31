module Wizard.Common.Components.DatePicker exposing
    ( datePicker
    , dateTimePicker
    , onChange
    , timePicker
    , value
    )

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as D
import Json.Encode as E


datePicker : List (Html.Attribute msg) -> Html msg
datePicker attributes =
    Html.node "date-picker" attributes []


dateTimePicker : List (Html.Attribute msg) -> Html msg
dateTimePicker attributes =
    Html.node "datetime-picker" attributes []


timePicker : List (Html.Attribute msg) -> Html msg
timePicker attributes =
    Html.node "time-picker" attributes []


onChange : (String -> msg) -> Html.Attribute msg
onChange toMsg =
    Html.Events.on "datePickerChanged" <|
        D.map toMsg <|
            D.at [ "target", "datePickerValue" ] <|
                D.string


value : String -> Html.Attribute msg
value v =
    Html.Attributes.property "datePickerValue" (E.string v)
