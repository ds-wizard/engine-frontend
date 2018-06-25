module Public.Questionnaire.View exposing (..)

import Html exposing (..)
import Msgs
import Public.Questionnaire.Models exposing (Model)
import Public.Questionnaire.Msgs exposing (Msg)


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    div [] [ text "Questionnaires" ]
