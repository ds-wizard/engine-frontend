module Common.Pages.KnowledgeModels.Components.KnowledgeModelActions exposing
    ( ActionsConfig
    , ExportMsg
    , KnowledgeModelPackageLike
    , actions
    , export
    , exportPot
    , exportUpdate
    )

{-| The actions of one knowledge model package, for a listing row and for the
detail page's header.

The actions are grouped, each visible under its `Feature` predicate. Navigation ("Open", "Preview", "Create project", …) is a link,
everything else a command, and "Delete" a danger action behind the version-aware
delete modal. Each action has a `data-cy` (`listing-item_action_…`).

Exporting asks the server for a download URL and then hands it to the browser;
`export`, `exportPot` and `exportUpdate` are the same two steps.

-}

import Common.Api.ApiError exposing (ApiError)
import Common.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Common.Api.Models.KnowledgeModelPackage.KnowledgeModelPackagePhase as KnowledgeModelPackagePhase exposing (KnowledgeModelPackagePhase)
import Common.Api.Models.UrlResponse as UrlResponse exposing (UrlResponse)
import Common.Api.Request as Request
import Common.Components.ItemActions as ItemActions
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.Feature as Feature
import Common.Data.RowPolicy exposing (RowPolicy)
import Common.Data.Session as Session
import Common.Ports.File as File
import Common.Routes as Routes
import Gettext exposing (gettext)
import Ui.Icon as Icon
import Uuid exposing (Uuid)


type alias KnowledgeModelPackageLike a =
    { a
        | uuid : Uuid
        , phase : KnowledgeModelPackagePhase
        , nonEditable : Bool
        , public : Bool
        , workspaceUuid : Maybe Uuid
    }


type alias ActionsConfig a msg =
    { exportMsg : KnowledgeModelPackageLike a -> msg
    , exportPotMsg : KnowledgeModelPackageLike a -> msg
    , updatePhaseMsg : KnowledgeModelPackageLike a -> KnowledgeModelPackagePhase -> msg
    , updatePublicMsg : KnowledgeModelPackageLike a -> Bool -> msg
    , deleteMsg : KnowledgeModelPackageLike a -> msg
    , viewActionVisible : Bool
    }


actions : RowPolicy -> AppState -> ActionsConfig a msg -> KnowledgeModelPackageLike a -> List (ItemActions.Action msg)
actions policy appState cfg kmPackage =
    let
        viewAction =
            ItemActions.link
                { icon = Icon.regular "fa-folder-open"
                , label = gettext "Open" appState.locale
                , url = Routes.toUrl appState (Routes.knowledgeModelsDetail kmPackage.uuid)
                }
                |> ItemActions.withTestId "listing-item_action_view"

        viewActionVisible =
            cfg.viewActionVisible && Feature.knowledgeModelsView appState

        previewAction =
            ItemActions.link
                { icon = Icon.solid "fa-eye"
                , label = gettext "Preview" appState.locale
                , url = Routes.toUrl appState (Routes.knowledgeModelsPreview kmPackage.uuid Nothing)
                }
                |> ItemActions.withTestId "listing-item_action_preview"

        compareAction =
            ItemActions.link
                { icon = Icon.solid "fa-table-columns"
                , label = gettext "Compare" appState.locale
                , url = Routes.toUrl appState (Routes.knowledgeModelsCompare (Just kmPackage.uuid))
                }
                |> ItemActions.withTestId "listing-item_action_compare"

        compareActionVisible =
            Feature.knowledgeModelsView appState && not kmPackage.nonEditable

        exportAction =
            ItemActions.action
                { icon = Icon.solid "fa-download"
                , label = gettext "Export" appState.locale
                , onSelect = cfg.exportMsg kmPackage
                }
                |> ItemActions.withTestId "listing-item_action_export"

        exportActionVisible =
            Feature.knowledgeModelsExport appState && not kmPackage.nonEditable

        exportPotAction =
            ItemActions.action
                { icon = Icon.solid "fa-language"
                , label = gettext "Export .pot file" appState.locale
                , onSelect = cfg.exportPotMsg kmPackage
                }
                |> ItemActions.withTestId "listing-item_action_export-pot"

        exportPotActionVisible =
            Feature.knowledgeModelsExportPot appState && not kmPackage.nonEditable

        createEditorAction =
            ItemActions.link
                { icon = Icon.solid "fa-pen-to-square"
                , label = gettext "Create KM editor" appState.locale
                , url = Routes.toUrl appState (Routes.kmEditorCreate (Just kmPackage.uuid) (Just True))
                }
                |> ItemActions.withTestId "listing-item_action_create-km-editor"

        forkAction =
            ItemActions.link
                { icon = Icon.solid "fa-code-branch"
                , label = gettext "Fork KM" appState.locale
                , url = Routes.toUrl appState (Routes.kmEditorCreate (Just kmPackage.uuid) Nothing)
                }
                |> ItemActions.withTestId "listing-item_action_fork"

        editorActionsVisible =
            Feature.knowledgeModelEditorsCreate appState && not kmPackage.nonEditable && policy.derive

        createProjectAction =
            ItemActions.link
                { icon = Icon.regular "fa-rectangle-list"
                , label = gettext "Create project" appState.locale
                , url = Routes.toUrl appState (Routes.projectsCreateFromKnowledgeModel kmPackage.uuid)
                }
                |> ItemActions.withTestId "listing-item_action_create-project"

        createProjectActionVisible =
            Session.exists appState.session && Feature.projectsCreateCustom appState && policy.derive

        setDeprecatedAction =
            ItemActions.action
                { icon = Icon.solid "fa-ban"
                , label = gettext "Set deprecated" appState.locale
                , onSelect = cfg.updatePhaseMsg kmPackage KnowledgeModelPackagePhase.Deprecated
                }
                |> ItemActions.withTestId "listing-item_action_set-deprecated"

        restoreAction =
            ItemActions.action
                { icon = Icon.solid "fa-rotate-left"
                , label = gettext "Restore" appState.locale
                , onSelect = cfg.updatePhaseMsg kmPackage KnowledgeModelPackagePhase.Released
                }
                |> ItemActions.withTestId "listing-item_action_restore"

        setPublicAction =
            ItemActions.action
                { icon = Icon.solid "fa-globe"
                , label = gettext "Set public" appState.locale
                , onSelect = cfg.updatePublicMsg kmPackage True
                }
                |> ItemActions.withTestId "listing-item_action_set-public"

        setPrivateAction =
            ItemActions.action
                { icon = Icon.solid "fa-lock"
                , label = gettext "Set private" appState.locale
                , onSelect = cfg.updatePublicMsg kmPackage False
                }
                |> ItemActions.withTestId "listing-item_action_set-private"

        -- Feature.knowledgeModelSetDeprecated, …Restore, …SetPublic and
        -- …SetPrivate, inlined: their annotations share one type variable
        -- between the app state and the package, so no package fits them.
        manage =
            Feature.knowledgeModelsManage appState && policy.manage kmPackage.workspaceUuid

        deleteAction =
            ItemActions.dangerAction
                { icon = Icon.solid "fa-trash"
                , label = gettext "Delete" appState.locale
                , onSelect = cfg.deleteMsg kmPackage
                }
                |> ItemActions.withTestId "listing-item_action_delete"
    in
    ItemActions.groups
        [ [ ( viewAction, viewActionVisible )
          , ( previewAction, Feature.knowledgeModelsPreview )
          , ( compareAction, compareActionVisible )
          ]
        , [ ( exportAction, exportActionVisible )
          , ( exportPotAction, exportPotActionVisible )
          ]
        , [ ( createEditorAction, editorActionsVisible )
          , ( forkAction, editorActionsVisible )
          , ( createProjectAction, createProjectActionVisible )
          ]
        , [ ( setDeprecatedAction, manage && kmPackage.phase == KnowledgeModelPackagePhase.Released )
          , ( restoreAction, manage && kmPackage.phase == KnowledgeModelPackagePhase.Deprecated )
          , ( setPublicAction, manage && not kmPackage.public )
          , ( setPrivateAction, manage && kmPackage.public )
          ]
        , [ ( deleteAction, Feature.knowledgeModelsDelete appState && policy.manage kmPackage.workspaceUuid )
          ]
        ]



-- EXPORT


{-| The answer to an export request: the URL the browser downloads from.
-}
type alias ExportMsg =
    Result ApiError UrlResponse


export : AppState -> Uuid -> (ExportMsg -> msg) -> Cmd msg
export appState kmPackageUuid =
    Request.get (AppState.toServerInfo appState)
        (KnowledgeModelPackagesApi.exportKnowledgeModelPackageUrl kmPackageUuid)
        UrlResponse.decoder


exportPot : AppState -> Uuid -> (ExportMsg -> msg) -> Cmd msg
exportPot appState kmPackageUuid =
    Request.get (AppState.toServerInfo appState)
        (KnowledgeModelPackagesApi.exportKnowledgeModelPackagePotUrl kmPackageUuid)
        UrlResponse.decoder


{-| Start the download once the URL is there. A failed request does nothing, as
it did in the wizard.
-}
exportUpdate : ExportMsg -> Cmd msg
exportUpdate result =
    case result of
        Ok urlResponse ->
            File.downloadFile urlResponse.url

        Err _ ->
            Cmd.none
