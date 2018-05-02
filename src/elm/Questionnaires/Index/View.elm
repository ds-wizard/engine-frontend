module Questionnaires.Index.View exposing (..)

import Common.Html exposing (detailContainerClass, emptyNode, linkTo)
import Common.View exposing (defaultFullPageError, fullPageActionResultView, fullPageLoader, pageHeader)
import Common.View.Table exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Msgs
import Questionnaires.Common.Models exposing (Questionnaire)
import Questionnaires.Index.Models exposing (Model)
import Questionnaires.Index.Msgs exposing (Msg(..))
import Questionnaires.Routing exposing (Route(Create, Detail))
import Routing


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    div [ class "questionnaires" ]
        [ pageHeader "Questionnaires" indexActions
        , fullPageActionResultView (indexTable tableConfig wrapMsg) model.questionnaires
        ]


indexActions : List (Html Msgs.Msg)
indexActions =
    [ linkTo (Routing.Questionnaires Create) [ class "btn btn-primary" ] [ text "Create questionnaire" ] ]


tableConfig : TableConfig Questionnaire Msg
tableConfig =
    { emptyMessage = "There are no questionnaires"
    , fields =
        [ { label = "Name"
          , getValue = TextValue .name
          }
        , { label = "Package ID"
          , getValue = TextValue .pkgId
          }
        ]
    , actions =
        [ { label = TableActionIcon "fa fa-trash-o"
          , action = TableActionMsg tableActionDelete
          }
        , { label = TableActionText "Fill questionnaire"
          , action = TableActionLink (Routing.Questionnaires << Detail << .uuid)
          }
        ]
    }


tableActionDelete : (Msg -> Msgs.Msg) -> Questionnaire -> Msgs.Msg
tableActionDelete wrapMsg =
    wrapMsg << ShowHideDeleteQuestionnaire << Just
