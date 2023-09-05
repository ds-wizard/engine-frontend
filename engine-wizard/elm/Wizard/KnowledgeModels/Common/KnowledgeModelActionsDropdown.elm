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
import Shared.Data.Package.PackagePhase as PackagePhase exposing (PackagePhase)
import Shared.Html exposing (faSet)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.ListingDropdown as ListingDropdown exposing (ListingActionType(..), ListingDropdownItem)
import Wizard.Common.Feature as Feature
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
                , icon = faSet "_global.open" appState
                , label = gettext "Open" appState.locale
                , msg = ListingActionLink (Routes.knowledgeModelsDetail package.id)
                , dataCy = "view"
                }

        viewActionVisible =
            cfg.viewActionVisible && Feature.knowledgeModelsView appState

        previewAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faSet "_global.preview" appState
                , label = gettext "Preview" appState.locale
                , msg = ListingActionLink (Routes.knowledgeModelsPreview package.id Nothing)
                , dataCy = "preview"
                }

        previewActionVisible =
            Feature.knowledgeModelsPreview appState

        exportAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faSet "_global.export" appState
                , label = gettext "Export" appState.locale
                , msg = ListingActionMsg (cfg.exportMsg package)
                , dataCy = "export"
                }

        exportActionVisible =
            Feature.knowledgeModelsExport appState && not package.nonEditable

        createEditorAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faSet "kmDetail.createKMEditor" appState
                , label = gettext "Create KM editor" appState.locale
                , msg = ListingActionLink (Routes.kmEditorCreate (Just package.id) (Just True))
                , dataCy = "create-km-editor"
                }

        createEditorActionVisible =
            Feature.knowledgeModelEditorsCreate appState && not package.nonEditable

        forkAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faSet "kmDetail.fork" appState
                , label = gettext "Fork KM" appState.locale
                , msg = ListingActionLink (Routes.kmEditorCreate (Just package.id) Nothing)
                , dataCy = "fork"
                }

        forkActionVisible =
            Feature.knowledgeModelEditorsCreate appState && not package.nonEditable

        createProjectAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faSet "kmDetail.createQuestionnaire" appState
                , label = gettext "Create project" appState.locale
                , msg = ListingActionLink (Routes.projectsCreateCustom (Just package.id))
                , dataCy = "create-project"
                }

        createProjectActionVisible =
            Feature.projectsCreateCustom appState

        setDeprecatedAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faSet "documentTemplate.setDeprecated" appState
                , label = gettext "Set deprecated" appState.locale
                , msg = ListingActionMsg (cfg.updatePhaseMsg package PackagePhase.Deprecated)
                , dataCy = "set-deprecated"
                }

        setDeprecatedActionVisible =
            package.phase == PackagePhase.Released

        restoreAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faSet "documentTemplate.restore" appState
                , label = gettext "Restore" appState.locale
                , msg = ListingActionMsg (cfg.updatePhaseMsg package PackagePhase.Released)
                , dataCy = "restore"
                }

        restoreActionVisible =
            package.phase == PackagePhase.Deprecated

        deleteAction =
            ListingDropdown.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faSet "_global.delete" appState
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
    ListingDropdown.dropdown appState
        { dropdownState = dropdownConfig.dropdownState
        , toggleMsg = dropdownConfig.toggleMsg
        , items = actions appState actionsConfig package
        }
