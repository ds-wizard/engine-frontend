module KnowledgeModels.Migration.View exposing (..)

import Html exposing (..)
import KnowledgeModels.Migration.Models exposing (Model)
import Msgs


view : Model -> Html Msgs.Msg
view model =
    div []
        [ text "Migration" ]
