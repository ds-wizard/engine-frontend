module Wizard.Pages.Documents.Common.DocumentPluginActions exposing (documentPluginActions)

import Common.Components.FontAwesome exposing (fa)
import Gettext exposing (gettext)
import Wizard.Api.Models.Document exposing (Document)
import Wizard.Components.ListingDropdown as ListingDropdown exposing (ListingActionType(..), ListingDropdownItem)
import Wizard.Components.PluginModal as PluginModal
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Plugins.Plugin as Plugin exposing (DocumentActionConnector, Plugin)


documentPluginActions : AppState -> Document -> (PluginModal.Msg Document -> msg) -> List ( ListingDropdownItem msg, Bool )
documentPluginActions appState document wrapMsg =
    AppState.getPluginsByConnector appState .documentActions
        |> Plugin.filterByDtPatterns document.documentTemplateId
        |> Plugin.filterByDtFormats document.format.uuid
        |> List.sortBy (.name << .action << Tuple.second)
        |> List.map (pluginAction appState document wrapMsg)


pluginAction :
    AppState
    -> Document
    -> (PluginModal.Msg Document -> msg)
    -> ( Plugin, DocumentActionConnector )
    -> ( ListingDropdownItem msg, Bool )
pluginAction appState document wrapMsg ( plugin, connector ) =
    ( ListingDropdown.dropdownAction
        { extraClass = Nothing
        , icon = fa connector.action.icon
        , label = gettext connector.action.name appState.locale
        , msg =
            ListingActionMsg <|
                wrapMsg <|
                    PluginModal.open
                        { pluginUuid = plugin.uuid
                        , pluginElement = connector.element
                        , data = document
                        }
        , dataCy = ""
        }
    , True
    )
