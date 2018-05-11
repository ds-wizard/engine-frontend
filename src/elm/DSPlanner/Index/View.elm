module DSPlanner.Index.View exposing (..)

import Common.Html exposing (detailContainerClass, emptyNode, linkTo)
import Common.View exposing (defaultFullPageError, fullPageActionResultView, fullPageLoader, modalView, pageHeader)
import Common.View.Forms exposing (formSuccessResultView)
import Common.View.Table exposing (..)
import DSPlanner.Common.Models exposing (Questionnaire)
import DSPlanner.Index.Models exposing (Model)
import DSPlanner.Index.Msgs exposing (Msg(..))
import DSPlanner.Routing exposing (Route(Create, Detail))
import Html exposing (..)
import Html.Attributes exposing (..)
import Msgs
import Routing


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    div [ class "questionnaires" ]
        [ pageHeader "Data Stewardship Planner" indexActions
        , formSuccessResultView model.deletingQuestionnaire
        , fullPageActionResultView (indexTable tableConfig wrapMsg) model.questionnaires
        , deleteModal wrapMsg model
        ]


indexActions : List (Html Msgs.Msg)
indexActions =
    [ linkTo (Routing.DSPlanner Create) [ class "btn btn-primary" ] [ text "Create" ] ]


tableConfig : TableConfig Questionnaire Msg
tableConfig =
    { emptyMessage = "There are no questionnaires"
    , fields =
        [ { label = "Name"
          , getValue = TextValue .name
          }
        , { label = "Package Name"
          , getValue = TextValue (.package >> .name)
          }
        , { label = "Package Version"
          , getValue = TextValue (.package >> .version)
          }
        , { label = "Package ID"
          , getValue = TextValue (.package >> .id)
          }
        ]
    , actions =
        [ { label = TableActionIcon "fa fa-trash-o"
          , action = TableActionMsg tableActionDelete
          }
        , { label = TableActionText "Fill questionnaire"
          , action = TableActionLink (Routing.DSPlanner << Detail << .uuid)
          }
        ]
    }


tableActionDelete : (Msg -> Msgs.Msg) -> Questionnaire -> Msgs.Msg
tableActionDelete wrapMsg =
    wrapMsg << ShowHideDeleteQuestionnaire << Just


deleteModal : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
deleteModal wrapMsg model =
    let
        ( visible, name ) =
            case model.questionnaireToBeDeleted of
                Just questionnaire ->
                    ( True, questionnaire.name )

                Nothing ->
                    ( False, "" )

        modalContent =
            [ p []
                [ text "Are you sure you want to permanently delete "
                , strong [] [ text name ]
                , text "?"
                ]
            ]

        modalConfig =
            { modalTitle = "Delete questionnaire"
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.deletingQuestionnaire
            , actionName = "Delete"
            , actionMsg = wrapMsg DeleteQuestionnaire
            , cancelMsg = wrapMsg <| ShowHideDeleteQuestionnaire Nothing
            }
    in
    modalView modalConfig
