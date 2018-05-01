module Questionnaires.Create.View exposing (..)

import Common.Html exposing (detailContainerClass)
import Common.View exposing (pageHeader)
import Html exposing (..)
import Msgs
import Questionnaires.Create.Models exposing (Model)
import Questionnaires.Create.Msgs exposing (Msg)


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    div [ detailContainerClass ]
        [ pageHeader "Create questionnaire" [] ]
