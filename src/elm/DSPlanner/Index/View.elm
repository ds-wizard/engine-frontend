module DSPlanner.Index.View exposing (deleteModal, exportAction, exportFormats, exportItem, getExportUrl, indexActions, tableActionDelete, tableConfig, view)

import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Common.Html exposing (detailContainerClass, emptyNode, linkTo)
import Common.View exposing (defaultFullPageError, fullPageActionResultView, fullPageLoader, modalView, pageHeader)
import Common.View.Forms exposing (formSuccessResultView)
import Common.View.Table exposing (..)
import DSPlanner.Common.Models exposing (Questionnaire)
import DSPlanner.Index.Models exposing (Model, QuestionnaireRow)
import DSPlanner.Index.Msgs exposing (Msg(..))
import DSPlanner.Routing exposing (Route(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Msgs
import Requests exposing (apiUrl)
import Routing


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    div [ class "col DSPlanner__Index" ]
        [ pageHeader "Data Stewardship Planner" indexActions
        , formSuccessResultView model.deletingQuestionnaire
        , fullPageActionResultView (indexTable (tableConfig model) wrapMsg) model.questionnaires
        , deleteModal wrapMsg model
        ]


indexActions : List (Html Msgs.Msg)
indexActions =
    [ linkTo (Routing.DSPlanner <| Create Nothing) [ class "btn btn-primary" ] [ text "Create" ] ]


tableConfig : Model -> TableConfig QuestionnaireRow Msg
tableConfig model =
    { emptyMessage = "There are no questionnaires"
    , fields =
        [ { label = "Name"
          , getValue = TextValue (.questionnaire >> .name)
          }
        , { label = "Package Name"
          , getValue = TextValue (.questionnaire >> .package >> .name)
          }
        , { label = "Package Version"
          , getValue = TextValue (.questionnaire >> .package >> .version)
          }
        , { label = "Package ID"
          , getValue = TextValue (.questionnaire >> .package >> .id)
          }
        ]
    , actions =
        [ { label = TableActionText "Fill questionnaire"
          , action = TableActionButtonLink (Routing.DSPlanner << Detail << .uuid << .questionnaire)
          , visible = always True
          }
        , { label = TableActionText ""
          , action = TableActionCustom exportAction
          , visible = always True
          }
        , { label = TableActionIcon "fa fa-trash-o"
          , action = TableActionMsg tableActionDelete
          , visible = always True
          }
        ]
    , sortBy = .questionnaire >> .name
    }


tableActionDelete : (Msg -> Msgs.Msg) -> QuestionnaireRow -> Msgs.Msg
tableActionDelete wrapMsg =
    wrapMsg << ShowHideDeleteQuestionnaire << Just << .questionnaire


exportFormats : List ( String, String )
exportFormats =
    [ ( "json", "JSON Data" )
    , ( "html", "HTML Document" )
    , ( "pdf", "PDF Document" )
    , ( "latex", "LaTeX Document" )
    , ( "docx", "MS Word Document" )
    , ( "odt", "OpenDocument Text" )
    , ( "markdown", "Markdown Document" )
    ]


exportAction : (Msg -> Msgs.Msg) -> QuestionnaireRow -> Html Msgs.Msg
exportAction wrapMsg questionnaireRow =
    Dropdown.dropdown questionnaireRow.dropdownState
        { options = [ Dropdown.alignMenuRight ]
        , toggleMsg = wrapMsg << DropdownMsg questionnaireRow.questionnaire
        , toggleButton = Dropdown.toggle [ Button.roleLink ] [ text "Export" ]
        , items = List.map (exportItem questionnaireRow.questionnaire) exportFormats
        }


exportItem : Questionnaire -> ( String, String ) -> Dropdown.DropdownItem msg
exportItem questionnaire ( format, formatLabel ) =
    Dropdown.anchorItem
        [ href <| getExportUrl format questionnaire, target "_blank" ]
        [ text formatLabel ]


getExportUrl : String -> Questionnaire -> String
getExportUrl format questionnaire =
    apiUrl "/questionnaires/" ++ questionnaire.uuid ++ "/dmp?format=" ++ format


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
    modalView modalConfig
