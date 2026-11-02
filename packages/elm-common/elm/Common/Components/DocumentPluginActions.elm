module Common.Components.DocumentPluginActions exposing (actions)

{-| The actions plugins add to a document — those whose document template and
format patterns match it — each opening the plugin's element in a dialog.
-}

import Common.Api.Models.Document exposing (Document)
import Common.Components.ItemActions as ItemActions
import Common.Components.PluginModal as PluginModal
import Common.Data.AppState as AppState exposing (AppState)
import Common.Plugins.Plugin as Plugin exposing (DocumentActionConnector, Plugin)
import Common.Utils.DocumentTemplateUtils as DocumentTemplateUtils
import Gettext exposing (gettext)
import Ui.Icon as Icon
import Uuid


actions : AppState -> Document -> (PluginModal.Msg Document -> msg) -> List ( ItemActions.Action msg, Bool )
actions appState document wrapMsg =
    AppState.getPluginsByConnector appState .documentActions
        |> Plugin.filterByDtPatterns (DocumentTemplateUtils.getId document.documentTemplate)
        |> Plugin.filterByDtFormats document.format.uuid
        |> List.sortBy (.name << .action << Tuple.second)
        |> List.map (pluginAction appState document wrapMsg)


pluginAction : AppState -> Document -> (PluginModal.Msg Document -> msg) -> ( Plugin, DocumentActionConnector ) -> ( ItemActions.Action msg, Bool )
pluginAction appState document wrapMsg ( plugin, connector ) =
    ( ItemActions.action
        { icon = Icon.fromClass connector.action.icon
        , label = gettext connector.action.name appState.locale
        , onSelect =
            wrapMsg
                (PluginModal.open
                    { pluginUuid = plugin.uuid
                    , pluginElement = connector.element
                    , data = document
                    }
                )
        }
        |> ItemActions.withTestId ("listing-item_action_plugin-" ++ Uuid.toString plugin.uuid)
    , True
    )
