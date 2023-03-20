module Wizard.DocumentTemplates.Common.DocumentTemplateActionsDropdown exposing
    ( ActionsConfig
    , DocumentTemplateLike
    , DropdownConfig
    , actions
    , dropdown
    )

import Bootstrap.Dropdown as Dropdown
import Gettext exposing (gettext)
import Html exposing (Html)
import Shared.Data.DocumentTemplate.DocumentTemplatePhase as DocumentTemplatePhase exposing (DocumentTemplatePhase)
import Shared.Html exposing (faSet)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.ListingDropdown as ListingDropdown exposing (ListingActionType(..), ListingDropdownItem)
import Wizard.Common.Feature as Feature
import Wizard.Routes as Routes


type alias DocumentTemplateLike a =
    { a
        | id : String
        , phase : DocumentTemplatePhase
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
                , icon = faSet "_global.view" appState
                , label = gettext "View detail" appState.locale
                , msg = ListingActionLink (Routes.documentTemplatesDetail template.id)
                , dataCy = "view"
                }

        viewActionVisible =
            cfg.viewActionVisible && Feature.documentTemplatesView appState

        exportAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faSet "_global.export" appState
                , label = gettext "Export" appState.locale
                , msg = ListingActionMsg (cfg.exportMsg template)
                , dataCy = "export"
                }

        exportActionVisible =
            Feature.documentTemplatesExport appState

        createEditorAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faSet "_global.edit" appState
                , label = gettext "Create editor" appState.locale
                , msg = ListingActionLink (Routes.documentTemplateEditorCreate (Just template.id) (Just True))
                , dataCy = "dt-detail_create-editor-link"
                }

        createEditorActionVisible =
            Feature.documentTemplatesView appState

        setDeprecatedAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faSet "documentTemplate.setDeprecated" appState
                , label = gettext "Set deprecated" appState.locale
                , msg = ListingActionMsg (cfg.updatePhaseMsg template DocumentTemplatePhase.Deprecated)
                , dataCy = "dt-detail_set-deprecated"
                }

        setDeprecatedActionVisible =
            template.phase == DocumentTemplatePhase.Released

        restoreAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faSet "documentTemplate.restore" appState
                , label = gettext "Restore" appState.locale
                , msg = ListingActionMsg (cfg.updatePhaseMsg template DocumentTemplatePhase.Released)
                , dataCy = "dt-detail_restore"
                }

        restoreActionVisible =
            template.phase == DocumentTemplatePhase.Deprecated

        deleteAction =
            ListingDropdown.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faSet "_global.delete" appState
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
    ListingDropdown.dropdown appState
        { dropdownState = dropdownConfig.dropdownState
        , toggleMsg = dropdownConfig.toggleMsg
        , items = actions appState actionsConfig documentTemplate
        }
