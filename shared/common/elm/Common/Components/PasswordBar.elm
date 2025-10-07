module Common.Components.PasswordBar exposing (view)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Rumkin


view : String -> Html msg
view value =
    let
        passwordClass =
            if String.isEmpty value then
                ""

            else
                case (Rumkin.getStats value).strength of
                    Rumkin.VeryWeak ->
                        "PasswordBar--VeryWeak"

                    Rumkin.Weak ->
                        "PasswordBar--Weak"

                    Rumkin.Reasonable ->
                        "PasswordBar--Reasonable"

                    Rumkin.Strong ->
                        "PasswordBar--Strong"

                    Rumkin.VeryStrong ->
                        "PasswordBar--VeryStrong"
    in
    div [ class ("PasswordBar " ++ passwordClass) ]
        [ div [] []
        , div [] []
        , div [] []
        , div [] []
        , div [] []
        ]
