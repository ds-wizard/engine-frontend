module Questionnaires.Index.View exposing (view)

import Common.AppState exposing (AppState)
import Common.Html exposing (linkTo)
import Common.View.FormResult as FormResult
import Common.View.Modal as Modal
import Common.View.Page as Page
import Common.View.Table as Table exposing (TableAction(..), TableActionLabel(..), TableConfig, TableFieldValue(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Msgs
import Questionnaires.Common.Models exposing (Questionnaire)
import Questionnaires.Index.ExportModal.View as ExportModal
import Questionnaires.Index.Models exposing (Model)
import Questionnaires.Index.Msgs exposing (Msg(..))
import Questionnaires.Routing exposing (Route(..))
import Routing


view : (Msg -> Msgs.Msg) -> AppState -> Model -> Html Msgs.Msg
view wrapMsg appState model =
    div [ class "col Questionnaires__Index" ]
        [ Page.header "Questionnaires" indexActions
        , FormResult.successOnlyView model.deletingQuestionnaire
        , Page.actionResultView (Table.view (tableConfig model) wrapMsg) model.questionnaires
        , ExportModal.view (wrapMsg << ExportModalMsg) appState model.exportModalModel
        , deleteModal wrapMsg model
        ]


indexActions : List (Html Msgs.Msg)
indexActions =
    [ linkTo (Routing.Questionnaires <| Create Nothing) [ class "btn btn-primary" ] [ text "Create" ] ]


tableConfig : Model -> TableConfig Questionnaire Msg
tableConfig model =
    { emptyMessage = "There are no questionnaires"
    , fields =
        [ { label = "Name"
          , getValue = TextValue .name
          }
        , { label = "Visibility"
          , getValue = HtmlValue tableFieldVisibility
          }
        , { label = "Knowledge Model"
          , getValue = HtmlValue tableFieldKnowledgeModel
          }
        ]
    , actions =
        [ { label = TableActionPrimary "Fill questionnaire"
          , action = TableActionLink (Routing.Questionnaires << Detail << .uuid)
          , visible = always True
          }
        , { label = TableActionDefault "download" "Export"
          , action = TableActionMsg tableActionExport
          , visible = always True
          }
        , { label = TableActionDefault "edit" "Edit"
          , action = TableActionLink (Routing.Questionnaires << Edit << .uuid)
          , visible = always True
          }
        , { label = TableActionDestructive "trash-o" "Delete"
          , action = TableActionMsg tableActionDelete
          , visible = always True
          }
        ]
    , sortBy = .name
    }


tableFieldVisibility : Questionnaire -> Html msg
tableFieldVisibility questionnaire =
    if questionnaire.private then
        span [ class "badge badge-danger" ]
            [ text "private" ]

    else
        span [ class "badge badge-info" ]
            [ text "public" ]


tableFieldKnowledgeModel : Questionnaire -> Html msg
tableFieldKnowledgeModel questionnaire =
    span []
        [ text questionnaire.package.name
        , text ", "
        , text questionnaire.package.version
        , text " ("
        , code [] [ text questionnaire.package.id ]
        , text ")"
        ]


tableActionExport : (Msg -> Msgs.Msg) -> Questionnaire -> Msgs.Msg
tableActionExport wrapMsg =
    wrapMsg << ShowExportQuestionnaire


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
            , cancelMsg = Just <| wrapMsg <| ShowHideDeleteQuestionnaire Nothing
            }
    in
    Modal.confirm modalConfig
