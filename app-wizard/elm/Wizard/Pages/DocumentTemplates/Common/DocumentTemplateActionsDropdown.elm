module Wizard.Pages.DocumentTemplates.Common.DocumentTemplateActionsDropdown exposing
    ( ActionsConfig
    , DocumentTemplateLike
    , DropdownConfig
    , actions
    , dropdown
    )

import Bootstrap.Dropdown as Dropdown
import Common.Components.FontAwesome exposing (faDelete, faDocumentTemplateRestore, faDocumentTemplateSetDeprecated, faEdit, faExport, faView)
import Gettext exposing (gettext)
import Html exposing (Html)
import Wizard.Api.Models.DocumentTemplate.DocumentTemplatePhase as DocumentTemplatePhase exposing (DocumentTemplatePhase)
import Wizard.Components.ListingDropdown as ListingDropdown exposing (ListingActionType(..), ListingDropdownItem)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Routes as Routes
import Wizard.Utils.Feature as Feature


type alias DocumentTemplateLike a =
    { a
        | id : String
        , phase : DocumentTemplatePhase
        , nonEditable : Bool
    }


type alias DropdownConfig msg =
    { dropdownState : Dropdown.State
    , toggleMsg : Dropdown.State -> msg
    }


type alias ActionsConfig a msg =
    { exportMsg : DocumentTemplateLike a -> msg
    , updatePhaseMsg : DocumentTemplateLike a -> DocumentTemplatePhase -> msg
    , deleteMsg : DocumentTemplateLike a -> msg
    , viewActionVisible : Bool
    }


actions : AppState -> ActionsConfig a msg -> DocumentTemplateLike a -> List (ListingDropdownItem msg)
actions appState cfg template =
    let
        viewAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faView
                , label = gettext "View detail" appState.locale
                , msg = ListingActionLink (Routes.documentTemplatesDetail template.id)
                , dataCy = "view"
                }

        viewActionVisible =
            cfg.viewActionVisible && Feature.documentTemplatesView appState

        exportAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faExport
                , label = gettext "Export" appState.locale
                , msg = ListingActionMsg (cfg.exportMsg template)
                , dataCy = "export"
                }

        exportActionVisible =
            Feature.documentTemplatesExport appState && not template.nonEditable

        createEditorAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faEdit
                , label = gettext "Create editor" appState.locale
                , msg = ListingActionLink (Routes.documentTemplateEditorCreate (Just template.id) (Just True))
                , dataCy = "create-editor"
                }

        createEditorActionVisible =
            Feature.documentTemplatesView appState && not template.nonEditable

        setDeprecatedAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faDocumentTemplateSetDeprecated
                , label = gettext "Set deprecated" appState.locale
                , msg = ListingActionMsg (cfg.updatePhaseMsg template DocumentTemplatePhase.Deprecated)
                , dataCy = "set-deprecated"
                }

        setDeprecatedActionVisible =
            template.phase == DocumentTemplatePhase.Released

        restoreAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faDocumentTemplateRestore
                , label = gettext "Restore" appState.locale
                , msg = ListingActionMsg (cfg.updatePhaseMsg template DocumentTemplatePhase.Released)
                , dataCy = "restore"
                }

        restoreActionVisible =
            template.phase == DocumentTemplatePhase.Deprecated

        deleteAction =
            ListingDropdown.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faDelete
                , label = gettext "Delete" appState.locale
                , msg = ListingActionMsg (cfg.deleteMsg template)
                , dataCy = "delete"
                }

        deleteActionVisible =
            Feature.documentTemplatesDelete appState

        groups =
            [ [ ( viewAction, viewActionVisible )
              , ( exportAction, exportActionVisible )
              ]
            , [ ( createEditorAction, createEditorActionVisible ) ]
            , [ ( setDeprecatedAction, setDeprecatedActionVisible )
              , ( restoreAction, restoreActionVisible )
              , ( deleteAction, deleteActionVisible )
              ]
            ]
    in
    ListingDropdown.itemsFromGroups groups


dropdown : AppState -> DropdownConfig msg -> ActionsConfig a msg -> DocumentTemplateLike a -> Html msg
dropdown appState dropdownConfig actionsConfig documentTemplate =
    ListingDropdown.dropdown
        { dropdownState = dropdownConfig.dropdownState
        , toggleMsg = dropdownConfig.toggleMsg
        , items = actions appState actionsConfig documentTemplate
        }
