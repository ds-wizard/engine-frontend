module KnowledgeModels.Migration.View exposing (..)

import Common.View exposing (pageHeader)
import Html exposing (..)
import Html.Attributes exposing (class)
import KnowledgeModels.Migration.Models exposing (Model)
import Msgs


view : Model -> Html Msgs.Msg
view model =
    div [ class "col-xs-12 col-lg-10 col-lg-offset-1" ]
        [ pageHeader "Migration" []
        ]
