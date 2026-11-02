module Common.Pages.DocumentTemplates.Components.DocumentTemplateActions exposing
    ( ActionsConfig
    , DocumentTemplateLike
    , actions
    , downloadCompleted
    , fetchDownloadUrl
    )

{-| The actions of a document template, for a listing row and the detail
header alike, grouped, each behind its permission, with a `data-cy`
(`listing-item_action_…`).
-}

import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.DocumentTemplate.DocumentTemplatePhase as DocumentTemplatePhase exposing (DocumentTemplatePhase)
import Common.Api.Models.UrlResponse as UrlResponse exposing (UrlResponse)
import Common.Api.Request as Request
import Common.Components.ItemActions as ItemActions exposing (Action)
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.Feature as Feature
import Common.Data.RowPolicy exposing (RowPolicy)
import Common.Ports.File as File
import Common.Routes as Routes
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Ui.Icon as Icon
import Uuid exposing (Uuid)


type alias DocumentTemplateLike a =
    { a
        | uuid : Uuid
        , phase : DocumentTemplatePhase
        , nonEditable : Bool
        , potFileReady : Bool
        , workspaceUuid : Maybe Uuid
    }


type alias ActionsConfig a msg =
    { exportMsg : DocumentTemplateLike a -> msg
    , exportPotMsg : DocumentTemplateLike a -> msg
    , updatePhaseMsg : DocumentTemplateLike a -> DocumentTemplatePhase -> msg
    , deleteMsg : DocumentTemplateLike a -> msg
    , viewActionVisible : Bool
    }


actions : RowPolicy -> AppState -> ActionsConfig a msg -> DocumentTemplateLike a -> List (Action msg)
actions policy appState cfg template =
    let
        viewAction =
            ItemActions.link
                { icon = Icon.regular "fa-eye"
                , label = gettext "View detail" appState.locale
                , url = Routes.toUrl appState (Routes.documentTemplatesDetail template.uuid)
                }
                |> ItemActions.withTestId "listing-item_action_view"

        viewActionVisible =
            cfg.viewActionVisible && Feature.documentTemplatesView appState

        exportAction =
            ItemActions.action
                { icon = Icon.solid "fa-download"
                , label = gettext "Export" appState.locale
                , onSelect = cfg.exportMsg template
                }
                |> ItemActions.withTestId "listing-item_action_export"

        exportActionVisible =
            Feature.documentTemplatesExport appState && not template.nonEditable

        exportPotAction =
            ItemActions.action
                { icon = Icon.solid "fa-language"
                , label = gettext "Export .pot file" appState.locale
                , onSelect = cfg.exportPotMsg template
                }
                |> ItemActions.withTestId "listing-item_action_export-pot"

        exportPotActionVisible =
            Feature.documentTemplatesExportPot appState && template.potFileReady

        createEditorAction =
            ItemActions.link
                { icon = Icon.solid "fa-pen-to-square"
                , label = gettext "Create editor" appState.locale
                , url = Routes.toUrl appState (Routes.documentTemplateEditorCreate (Just template.uuid) (Just True))
                }
                |> ItemActions.withTestId "listing-item_action_create-editor"

        createEditorActionVisible =
            Feature.documentTemplateEditorsCreate appState && not template.nonEditable && policy.derive

        setDeprecatedAction =
            ItemActions.action
                { icon = Icon.solid "fa-ban"
                , label = gettext "Set deprecated" appState.locale
                , onSelect = cfg.updatePhaseMsg template DocumentTemplatePhase.Deprecated
                }
                |> ItemActions.withTestId "listing-item_action_set-deprecated"

        setDeprecatedActionVisible =
            Feature.documentTemplatesManage appState && template.phase == DocumentTemplatePhase.Released && policy.manage template.workspaceUuid

        restoreAction =
            ItemActions.action
                { icon = Icon.solid "fa-rotate-left"
                , label = gettext "Restore" appState.locale
                , onSelect = cfg.updatePhaseMsg template DocumentTemplatePhase.Released
                }
                |> ItemActions.withTestId "listing-item_action_restore"

        restoreActionVisible =
            Feature.documentTemplatesManage appState && template.phase == DocumentTemplatePhase.Deprecated && policy.manage template.workspaceUuid

        deleteAction =
            ItemActions.dangerAction
                { icon = Icon.solid "fa-trash"
                , label = gettext "Delete" appState.locale
                , onSelect = cfg.deleteMsg template
                }
                |> ItemActions.withTestId "listing-item_action_delete"

        deleteActionVisible =
            Feature.documentTemplatesDelete appState && policy.manage template.workspaceUuid
    in
    ItemActions.groups
        [ [ ( viewAction, viewActionVisible )
          , ( exportAction, exportActionVisible )
          , ( exportPotAction, exportPotActionVisible )
          ]
        , [ ( createEditorAction, createEditorActionVisible ) ]
        , [ ( setDeprecatedAction, setDeprecatedActionVisible )
          , ( restoreAction, restoreActionVisible )
          , ( deleteAction, deleteActionVisible )
          ]
        ]


{-| Export and export-pot answer with a short-lived URL to the file, which is
then downloaded.
-}
fetchDownloadUrl : AppState -> String -> (Result ApiError UrlResponse -> msg) -> Cmd msg
fetchDownloadUrl appState url =
    Request.get (AppState.toServerInfo appState) url UrlResponse.decoder


downloadCompleted : msg -> Result ApiError UrlResponse -> Cmd msg
downloadCompleted logoutMsg result =
    case result of
        Ok urlResponse ->
            File.downloadFile urlResponse.url

        Err _ ->
            RequestHelpers.getResultCmd logoutMsg result
