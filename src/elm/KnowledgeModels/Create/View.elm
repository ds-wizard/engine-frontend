module KnowledgeModels.Create.View exposing (..)

import Common.Forms exposing (formActions, inputSelect, inputText)
import Common.View exposing (pageHeader)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Msgs exposing (Msg)


view : Html Msg
view =
    div [ class "col-xs-12 col-lg-10 col-lg-offset-1" ]
        [ pageHeader "Create Knowledge Model" []
        , inputText "Name"
        , inputSelect "Parent" [ "Core 1.1", "Core 1.2" ]
        , formActions
        ]
