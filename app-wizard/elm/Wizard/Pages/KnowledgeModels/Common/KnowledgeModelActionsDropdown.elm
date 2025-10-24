module Wizard.Pages.KnowledgeModels.Common.KnowledgeModelActionsDropdown exposing
    ( ActionsConfig
    , DropdownConfig
    , KnowledgeModelPackageLike
    , actions
    , dropdown
    )

import Bootstrap.Dropdown as Dropdown
import Common.Components.FontAwesome exposing (faDelete, faDocumentTemplateRestore, faDocumentTemplateSetDeprecated, faExport, faKmDetailCreateKmEditor, faKmDetailCreateQuestionnaire, faKmDetailFork, faOpen, faPreview)
import Gettext exposing (gettext)
import Html exposing (Html)
import Wizard.Api.Models.KnowledgeModelPackage.KnowledgeModelPackagePhase as KnowledgeModelPackagePhase exposing (KnowledgeModelPackagePhase)
import Wizard.Components.ListingDropdown as ListingDropdown exposing (ListingActionType(..), ListingDropdownItem)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Data.Session as Session
import Wizard.Routes as Routes
import Wizard.Utils.Feature as Feature


type alias KnowledgeModelPackageLike a =
    { a
        | id : String
        , phase : KnowledgeModelPackagePhase
        , nonEditable : Bool
    }


type alias DropdownConfig msg =
    { dropdownState : Dropdown.State
    , toggleMsg : Dropdown.State -> msg
    }


type alias ActionsConfig a msg =
    { exportMsg : KnowledgeModelPackageLike a -> msg
    , updatePhaseMsg : KnowledgeModelPackageLike a -> KnowledgeModelPackagePhase -> msg
    , deleteMsg : KnowledgeModelPackageLike a -> msg
    , viewActionVisible : Bool
    }


actions : AppState -> ActionsConfig a msg -> KnowledgeModelPackageLike a -> List (ListingDropdownItem msg)
actions appState cfg kmPackage =
    let
        viewAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faOpen
                , label = gettext "Open" appState.locale
                , msg = ListingActionLink (Routes.knowledgeModelsDetail kmPackage.id)
                , dataCy = "view"
                }

        viewActionVisible =
            cfg.viewActionVisible && Feature.knowledgeModelsView appState

        previewAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faPreview
                , label = gettext "Preview" appState.locale
                , msg = ListingActionLink (Routes.knowledgeModelsPreview kmPackage.id Nothing)
                , dataCy = "preview"
                }

        previewActionVisible =
            Feature.knowledgeModelsPreview appState

        exportAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faExport
                , label = gettext "Export" appState.locale
                , msg = ListingActionMsg (cfg.exportMsg kmPackage)
                , dataCy = "export"
                }

        exportActionVisible =
            Feature.knowledgeModelsExport appState && not kmPackage.nonEditable

        createEditorAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faKmDetailCreateKmEditor
                , label = gettext "Create KM editor" appState.locale
                , msg = ListingActionLink (Routes.kmEditorCreate (Just kmPackage.id) (Just True))
                , dataCy = "create-km-editor"
                }

        createEditorActionVisible =
            Feature.knowledgeModelEditorsCreate appState && not kmPackage.nonEditable

        forkAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faKmDetailFork
                , label = gettext "Fork KM" appState.locale
                , msg = ListingActionLink (Routes.kmEditorCreate (Just kmPackage.id) Nothing)
                , dataCy = "fork"
                }

        forkActionVisible =
            Feature.knowledgeModelEditorsCreate appState && not kmPackage.nonEditable

        createProjectAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faKmDetailCreateQuestionnaire
                , label = gettext "Create project" appState.locale
                , msg = ListingActionLink (Routes.projectsCreateFromKnowledgeModel kmPackage.id)
                , dataCy = "create-project"
                }

        createProjectActionVisible =
            Session.exists appState.session && Feature.projectsCreateCustom appState

        setDeprecatedAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faDocumentTemplateSetDeprecated
                , label = gettext "Set deprecated" appState.locale
                , msg = ListingActionMsg (cfg.updatePhaseMsg kmPackage KnowledgeModelPackagePhase.Deprecated)
                , dataCy = "set-deprecated"
                }

        setDeprecatedActionVisible =
            Feature.knowledgeModelSetDeprecated appState kmPackage

        restoreAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faDocumentTemplateRestore
                , label = gettext "Restore" appState.locale
                , msg = ListingActionMsg (cfg.updatePhaseMsg kmPackage KnowledgeModelPackagePhase.Released)
                , dataCy = "restore"
                }

        restoreActionVisible =
            Feature.knowledgeModelRestore appState kmPackage

        deleteAction =
            ListingDropdown.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faDelete
                , label = gettext "Delete" appState.locale
                , msg = ListingActionMsg (cfg.deleteMsg kmPackage)
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


dropdown : AppState -> DropdownConfig msg -> ActionsConfig a msg -> KnowledgeModelPackageLike a -> Html msg
dropdown appState dropdownConfig actionsConfig kmPackage =
    ListingDropdown.dropdown
        { dropdownState = dropdownConfig.dropdownState
        , toggleMsg = dropdownConfig.toggleMsg
        , items = actions appState actionsConfig kmPackage
        }
