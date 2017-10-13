module Common.Forms exposing (..)

import Html exposing (Html, button, div, input, label, option, select, text, textarea)
import Html.Attributes exposing (class, rows, type_)
import Msgs exposing (Msg)


-- Inputs


inputText : String -> Html Msg
inputText labelText =
    div [ class "form-group" ]
        [ label [ class "control-label" ] [ text labelText ]
        , input [ class "form-control", type_ "text" ] []
        ]


inputTextarea : String -> Html Msg
inputTextarea labelText =
    div [ class "form-group" ]
        [ label [ class "control-label" ] [ text labelText ]
        , textarea [ class "form-control", rows 4 ] []
        ]


inputSelect : String -> List String -> Html Msg
inputSelect labelText options =
    div [ class "form-group" ]
        [ label [ class "control-label" ] [ text labelText ]
        , select [ class "form-control" ] (List.map inputSelectOption options)
        ]


inputSelectOption : String -> Html Msg
inputSelectOption labelText =
    option [] [ text labelText ]



-- Form Actions


formActions : Html Msg
formActions =
    div [ class "form-actions" ]
        [ button [ class "btn btn-default" ] [ text "Cancel" ]
        , button [ class "btn btn-primary" ] [ text "Save" ]
        ]
