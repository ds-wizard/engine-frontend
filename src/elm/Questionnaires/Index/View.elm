module Questionnaires.Index.View exposing (..)

import Common.Html exposing (detailContainerClass, linkTo)
import Common.View exposing (pageHeader)
import Html exposing (..)
import Html.Attributes exposing (..)
import Msgs
import Questionnaires.Index.Models exposing (Model)
import Questionnaires.Index.Msgs exposing (Msg)
import Questionnaires.Routing exposing (Route(Create))
import Routing


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    div [ class "user-management" ]
        [ pageHeader "Questionnaires" indexActions
        ]


indexActions : List (Html Msgs.Msg)
indexActions =
    [ linkTo (Routing.Questionnaires Create) [ class "btn btn-primary" ] [ text "Create questionnaire" ] ]
