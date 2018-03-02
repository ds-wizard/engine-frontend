module Public.ForgottenPassword.View exposing (view)

import Html exposing (..)
import Msgs


view : Html Msgs.Msg
view =
    div []
        [ text "Forgotten Password" ]
