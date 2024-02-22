module Registry2.Components.FontAwesome exposing (fas)

import Html exposing (Html, i)
import Html.Attributes exposing (class)


fas : String -> Html msg
fas icon =
    i [ class ("fas " ++ icon) ] []
