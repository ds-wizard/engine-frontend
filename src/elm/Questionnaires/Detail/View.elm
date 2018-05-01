module Questionnaires.Detail.View exposing (..)

import Common.Html exposing (detailContainerClass)
import Common.View exposing (pageHeader)
import Html exposing (..)
import Msgs
import Questionnaires.Detail.Models exposing (Model)
import Questionnaires.Detail.Msgs exposing (Msg)


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    div [ detailContainerClass ]
        [ pageHeader "Questionnaire Detail" [] ]
