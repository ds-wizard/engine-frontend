module Wizard.KnowledgeModels.Common.KnowledgeModelActionsDropdown exposing
    ( ActionsConfig
    , DropdownConfig
    , PackageLike
    , actions
    , dropdown
    )

import Bootstrap.Dropdown as Dropdown
import Gettext exposing (gettext)
import Html exposing (Html)
import Shared.Components.FontAwesome exposing (faDelete, faDocumentTemplateRestore, faDocumentTemplateSetDeprecated, faExport, faKmDetailCreateKmEditor, faKmDetailCreateQuestionnaire, faKmDetailFork, faOpen, faPreview)
import Wizard.Api.Models.Package.PackagePhase as PackagePhase exposing (PackagePhase)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.ListingDropdown as ListingDropdown exposing (ListingActionType(..), ListingDropdownItem)
import Wizard.Common.Feature as Feature
import Wizard.Data.Session as Session
import Wizard.Routes as Routes


type alias PackageLike a =
    { a
        | id : String
        , phase : PackagePhase
        , nonEditable : Bool
    }


type alias DropdownConfig msg =
    { dropdownState : Dropdown.State
    , toggleMsg : Dropdown.State -> msg
    }


type alias ActionsConfig a msg =
    { exportMsg : PackageLike a -> msg
    , updatePhaseMsg : PackageLike a -> PackagePhase -> msg
    , deleteMsg : PackageLike a -> msg
    , viewActionVisible : Bool
    }


actions : AppState -> ActionsConfig a msg -> PackageLike a -> List (ListingDropdownItem msg)
actions appState cfg package =
    let
        viewAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faOpen
                , label = gettext "Open" appState.locale
                , msg = ListingActionLink (Routes.knowledgeModelsDetail package.id)
                , dataCy = "view"
                }

        viewActionVisible =
            cfg.viewActionVisible && Feature.knowledgeModelsView appState

        previewAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faPreview
                , label = gettext "Preview" appState.locale
                , msg = ListingActionLink (Routes.knowledgeModelsPreview package.id Nothing)
                , dataCy = "preview"
                }

        previewActionVisible =
            Feature.knowledgeModelsPreview appState

        exportAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faExport
                , label = gettext "Export" appState.locale
                , msg = ListingActionMsg (cfg.exportMsg package)
                , dataCy = "export"
                }

        exportActionVisible =
            Feature.knowledgeModelsExport appState && not package.nonEditable

        createEditorAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faKmDetailCreateKmEditor
                , label = gettext "Create KM editor" appState.locale
                , msg = ListingActionLink (Routes.kmEditorCreate (Just package.id) (Just True))
                , dataCy = "create-km-editor"
                }

        createEditorActionVisible =
            Feature.knowledgeModelEditorsCreate appState && not package.nonEditable

        forkAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faKmDetailFork
                , label = gettext "Fork KM" appState.locale
                , msg = ListingActionLink (Routes.kmEditorCreate (Just package.id) Nothing)
                , dataCy = "fork"
                }

        forkActionVisible =
            Feature.knowledgeModelEditorsCreate appState && not package.nonEditable

        createProjectAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faKmDetailCreateQuestionnaire
                , label = gettext "Create project" appState.locale
                , msg = ListingActionLink (Routes.projectsCreateFromKnowledgeModel package.id)
                , dataCy = "create-project"
                }

        createProjectActionVisible =
            Session.exists appState.session && Feature.projectsCreateCustom appState

        setDeprecatedAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faDocumentTemplateSetDeprecated
                , label = gettext "Set deprecated" appState.locale
                , msg = ListingActionMsg (cfg.updatePhaseMsg package PackagePhase.Deprecated)
                , dataCy = "set-deprecated"
                }

        setDeprecatedActionVisible =
            Feature.knowledgeModelSetDeprecated appState package

        restoreAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faDocumentTemplateRestore
                , label = gettext "Restore" appState.locale
                , msg = ListingActionMsg (cfg.updatePhaseMsg package PackagePhase.Released)
                , dataCy = "restore"
                }

        restoreActionVisible =
            Feature.knowledgeModelRestore appState package

        deleteAction =
            ListingDropdown.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faDelete
                , label = gettext "Delete" appState.locale
                , msg = ListingActionMsg (cfg.deleteMsg package)
                , dataCy = "delete"
                }

        deleteActionVisible =
            Feature.knowledgeModelsDelete appState

        groups =
            [ [ ( viewAction, viewActionVisible )
              , ( previewAction, previewActionVisible )
              , ( exportAction, exportActionVisible )
              ]
            , [ ( createEditorAction, createEditorActionVisible )
              , ( forkAction, forkActionVisible )
              , ( createProjectAction, createProjectActionVisible )
              ]
            , [ ( setDeprecatedAction, setDeprecatedActionVisible )
              , ( restoreAction, restoreActionVisible )
              , ( deleteAction, deleteActionVisible )
              ]
            ]
    in
    ListingDropdown.itemsFromGroups groups


dropdown : AppState -> DropdownConfig msg -> ActionsConfig a msg -> PackageLike a -> Html msg
dropdown appState dropdownConfig actionsConfig package =
    ListingDropdown.dropdown
        { dropdownState = dropdownConfig.dropdownState
        , toggleMsg = dropdownConfig.toggleMsg
        , items = actions appState actionsConfig package
        }
